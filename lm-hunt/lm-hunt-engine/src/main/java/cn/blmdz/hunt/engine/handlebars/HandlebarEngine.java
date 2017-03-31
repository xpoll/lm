package cn.blmdz.hunt.engine.handlebars;

import java.io.FileNotFoundException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;

import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.HandlebarsException;
import com.github.jknack.handlebars.Helper;
import com.github.jknack.handlebars.Template;
import com.github.jknack.handlebars.io.TemplateLoader;
import com.google.common.base.Optional;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import cn.blmdz.hunt.common.UserNotLoginException;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.hunt.engine.Setting;
import cn.blmdz.hunt.engine.ThreadVars;
import cn.blmdz.hunt.engine.config.FileLoaderHelper;
import cn.blmdz.hunt.engine.mapping.Invoker;
import cn.blmdz.hunt.engine.service.ConfigService;

@Component
public class HandlebarsEngine
  implements ApplicationContextAware
{
  private static final Logger log = LoggerFactory.getLogger(HandlebarsEngine.class);
  private Handlebars handlebars;
  private Invoker invoker;
  private final LoadingCache<String, Optional<Template>> caches;
  private ConfigService configService;
  private List<ContextHandler> handlers;
  private ApplicationContext context;

  @Autowired(required=false)
  public HandlebarsEngine(Invoker invoker, Setting setting, ConfigService configService, FileLoaderHelper fileLoaderHelper)
  {
    this.invoker = invoker;
    TemplateLoader templateLoader = new GreatTemplateLoader(fileLoaderHelper);
    this.handlebars = new Handlebars(templateLoader);
    this.caches = initCache(!setting.isDevMode());
    this.configService = configService;
  }

  @PostConstruct
  public void init()
  {
    Map contextHandlers = this.context.getBeansOfType(ContextHandler.class);
    if ((contextHandlers == null) || (contextHandlers.isEmpty())) {
      this.handlers = Collections.emptyList();
    } else {
      this.handlers = Lists.newArrayListWithCapacity(contextHandlers.size());
      for (Map.Entry entry : contextHandlers.entrySet())
        this.handlers.add(entry.getValue());
    }
  }

  private LoadingCache<String, Optional<Template>> initCache(boolean buildCache)
  {
    if (buildCache) {
      return CacheBuilder.newBuilder().expireAfterWrite(5L, TimeUnit.MINUTES).build(new CacheLoader()
      {
        public Optional<Template> load(String path) throws Exception {
          Template t = null;
          String realPath = path.split("]")[1];
          try {
            t = HandlebarsEngine.this.handlebars.compile(realPath);
          } catch (Exception e) {
            HandlebarsEngine.log.error("failed to compile template(path={}), cause:{}", path, e.getMessage());
          }
          return Optional.fromNullable(t);
        }
      });
    }
    return null;
  }

  public <T> void registerHelper(String name, Helper<T> helper) {
    this.handlebars.registerHelper(name, helper);
  }

  public void registerHelpers(Map<String, Helper<?>> helpers) {
    for (String key : helpers.keySet())
      this.handlebars.registerHelper(key, (Helper)helpers.get(key));
  }

  public String execInline(String templateStr, Object context)
  {
    return execInline(templateStr, context, null);
  }

  public String execInline(String templateStr, Object params, String cacheKey) {
    try {
      if (params == null)
        params = Maps.newHashMap();
      Template template;
      Template template;
      if ((this.caches == null) || (cacheKey == null))
        template = this.handlebars.compileInline(templateStr);
      else {
        template = (Template)((Optional)this.caches.getUnchecked("inline/" + cacheKey)).orNull();
      }
      if (template == null) {
        log.error("failed to exec handlebars' template:{}", templateStr);
        return "";
      }
      return template.apply(params);
    } catch (Exception e) {
      log.error("exec handlebars' template failed: {},cause:{}", templateStr, Throwables.getStackTraceAsString(e));
    }return "";
  }

  public String execPath(String path, Map<String, Object> params, boolean isComponent)
    throws FileNotFoundException
  {
    try
    {
      if (params == null)
        params = Maps.newHashMap();
      Template template;
      if (isComponent) {
        String componentViewPath = "component:" + path + "/view";
        Template template;
        Template template;
        if (this.caches == null)
          template = this.handlebars.compile(componentViewPath);
        else {
          template = (Template)((Optional)this.caches.getUnchecked(getAppPathKey(componentViewPath))).orNull();
        }
        if (template == null) {
          log.error("failed to exec handlebars template:path={}", path);
          return "";
        }
        params.put("_COMP_PATH_", path);
      }
      else
      {
        Template template;
        if (this.caches == null)
          template = this.handlebars.compile(path);
        else {
          template = (Template)((Optional)this.caches.getUnchecked(getAppPathKey(path))).orNull();
        }
        if (template == null) {
          throw new FileNotFoundException("view not found: " + path);
        }
      }

      params.put("_USER_", UserUtil.getCurrentUser());
      params.put("_HREF_", this.configService.getFrontConfig(ThreadVars.getAppKey()).getCurrentHrefs(ThreadVars.getHost()));
      params.put("_LOCALE_", ThreadVars.getLocale().toString());
      return template.apply(params);
    }
    catch (Exception e) {
      Throwables.propagateIfInstanceOf(e, FileNotFoundException.class);
      if (e instanceof HandlebarsException) {
        Throwables.propagateIfInstanceOf(e.getCause(), FileNotFoundException.class);
        Throwables.propagateIfInstanceOf(e.getCause(), UserNotLoginException.class);
      }
      log.error("failed to execute handlebars' template(path={}),cause:{} ", path, 
        Throwables.getStackTraceAsString(e));
    }

    return "";
  }

  public String execComponent(io.terminus.pampas.engine.config.model.Component component, Map<String, Object> context) {
    Map services = (component.getServices() == null) ? new HashMap() : component.getServices();
    if (!Strings.isNullOrEmpty(component.getService()))
      services.put("_DATA_", component.getService());
    Map invokeResults;
    if (!services.isEmpty()) {
      invokeResults = new HashMap();
      try {
        for (String key : services.keySet()) {
          Object result = this.invoker.invoke((String)services.get(key), context);
          invokeResults.put(key, result);
        }
      } catch (UserNotLoginException e) {
        log.error("user doesn't login when invoke component, component:{}", component);
        if (context.get("_DESIGN_MODE_") == null)
          throw e;
      }
      catch (Exception e) {
        log.error("error when invoke component, component: {}", component, e);
        context.put("_ERROR_", e.getMessage());
      }
      context.putAll(invokeResults);
    }
    try {
      if ((this.handlers != null) && (!this.handlers.isEmpty())) {
        for (ContextHandler handler : this.handlers) {
          handler.handle(component, context);
        }
      }
      return execPath(component.getPath(), context, true);
    } catch (Exception e) {
      log.error("failed to execute handlebars' template(path={}),cause:{} ", component
        .getPath(), Throwables.getStackTraceAsString(e));
    }
    return "";
  }

  private String getAppPathKey(String path) {
    return "[" + ThreadVars.getAppKey() + "]" + path;
  }

  public void setApplicationContext(ApplicationContext applicationContext)
    throws BeansException
  {
    this.context = applicationContext;
  }

  public void setHandlebars(Handlebars handlebars)
  {
    this.handlebars = handlebars; } 
  public Handlebars getHandlebars() { return this.handlebars; }

  public void setInvoker(Invoker invoker) {
    this.invoker = invoker; } 
  public Invoker getInvoker() { return this.invoker; }

  public LoadingCache<String, Optional<Template>> getCaches() {
    return this.caches;
  }
  public void setConfigService(ConfigService configService) {
    this.configService = configService; } 
  public ConfigService getConfigService() { return this.configService; }

  public void setHandlers(List<ContextHandler> handlers) {
    this.handlers = handlers; } 
  public List<ContextHandler> getHandlers() { return this.handlers; }

  public void setContext(ApplicationContext context) {
    this.context = context; } 
  public ApplicationContext getContext() { return this.context; }

}