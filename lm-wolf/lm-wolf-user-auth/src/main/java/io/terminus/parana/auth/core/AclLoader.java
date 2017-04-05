package io.terminus.parana.auth.core;

import com.google.common.base.Throwables;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.Maps;
import io.terminus.parana.auth.core.AuthenticationProperties;
import io.terminus.parana.auth.core.LoadConfig;
import io.terminus.parana.auth.model.Acl;
import io.terminus.parana.auth.model.App;
import io.terminus.parana.auth.util.ParanaFileLoader;
import io.terminus.parana.auth.util.ParanaFileLoaderHelper;
import io.terminus.parana.auth.util.YAML;
import java.util.Date;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AclLoader implements Runnable {
   private static final Logger log = LoggerFactory.getLogger(AclLoader.class);
   public static final String ACL_FILE_NAME = "acl.yaml";
   private LoadingCache aclCache;
   private final ConcurrentMap loadConfigMap = Maps.newConcurrentMap();
   private final ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
   private final ParanaFileLoaderHelper paranaFileLoaderHelper;

   public AclLoader(AuthenticationProperties authenticationProperties, final ParanaFileLoaderHelper paranaFileLoaderHelper) {
      this.paranaFileLoaderHelper = paranaFileLoaderHelper;
      this.aclCache = CacheBuilder.newBuilder().build(new CacheLoader() {
         public Acl load(App app) throws Exception {
            String path = AclLoader.this.getAclFile(app);
            ParanaFileLoader.Resp resp = paranaFileLoaderHelper.load(path);
            LoadConfig config = new LoadConfig();
            config.setLocation(path);
            config.setSign(resp.getSign());
            config.setLoadedAt(new Date());
            AclLoader.this.loadConfigMap.put(path, config);
            if(resp.isNotFound()) {
               AclLoader.log.error("auth file not found, app={}, path={}", app.getKey(), path);
               throw new RuntimeException("auth file not found");
            } else {
               return AclLoader.this.parseAcl(resp.asString());
            }
         }
      });
      if(authenticationProperties.isDevMode()) {
         this.executorService.scheduleAtFixedRate(this, 5L, 5L, TimeUnit.SECONDS);
      } else {
         this.executorService.scheduleAtFixedRate(this, 5L, 5L, TimeUnit.MINUTES);
      }

   }

   public Acl getAcl(App app) {
      return (Acl)this.aclCache.getUnchecked(app);
   }

   private Acl parseAcl(String content) {
      try {
         return (Acl)YAML.mapper().readValue(content, Acl.class);
      } catch (Exception var3) {
         log.error("parse auth config failed, cause:{}|", Throwables.getStackTraceAsString(var3));
         throw new RuntimeException("auth file parse fail");
      }
   }

   private String getAclFile(App app) {
      return app.getAssetsHome() + "acl.yaml";
   }

   private void checkIfNeedReload() {
      for(Entry<String, LoadConfig> entry : this.loadConfigMap.entrySet()) {
         String path = (String)entry.getKey();
         LoadConfig config = (LoadConfig)entry.getValue();
         ParanaFileLoader.Resp resp = this.paranaFileLoaderHelper.load(path, config.getSign());
         if(resp.modified()) {
            log.info("auth tree config file changed {}", path);
            config.setLocation(path);
            config.setSign(resp.getSign());
            config.setLoadedAt(new Date());
            this.aclCache.invalidateAll();
            break;
         }
      }

   }

   public void run() {
      this.checkIfNeedReload();
   }
}
