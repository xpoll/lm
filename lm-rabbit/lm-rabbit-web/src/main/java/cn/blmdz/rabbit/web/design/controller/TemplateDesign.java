package cn.blmdz.rabbit.web.design.controller;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.google.common.base.MoreObjects;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import cn.blmdz.hunt.design.container.DPageRender;
import cn.blmdz.hunt.design.medol.Page;
import cn.blmdz.hunt.design.medol.Site;
import cn.blmdz.hunt.design.service.SiteService;
import cn.blmdz.hunt.engine.MessageSources;
import cn.blmdz.hunt.engine.RenderConstants;
import cn.blmdz.hunt.engine.ThreadVars;
import cn.blmdz.hunt.engine.config.model.Render;
import cn.blmdz.hunt.engine.exception.NotFound404Exception;
import cn.blmdz.hunt.engine.service.ConfigService;
import cn.blmdz.hunt.engine.utils.Domains;
import cn.blmdz.rabbit.web.design.ext.TemplateDesignHook;
import cn.blmdz.rabbit.web.design.modal.EcpAppKey;
import cn.blmdz.rabbit.web.design.modal.EcpLayoutType;
import cn.blmdz.rabbit.web.design.modal.Template;
import cn.blmdz.rabbit.web.design.modal.TemplatePageCategory;
import cn.blmdz.rabbit.web.design.service.EcpTemplateService;
import lombok.extern.slf4j.Slf4j;

/**
 * Author:Guo Chaopeng
 * Created on 3/2/15.
 */
@RequestMapping("/design/shop-templates")
@Controller
@Slf4j
public class TemplateDesign {

    @Autowired
    private EcpTemplateService templateService;

    @Autowired
    private ConfigService configService;

    @Autowired(required = false)
    private DPageRender dPageRender;

    @Autowired(required = false)
    private SiteService siteService;

    @Autowired(required = false)
    private TemplateDesignHook templateDesignHook;

    @Autowired
    private MessageSources messageSources;

    @RequestMapping(value = "/{key}/design", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    public String design(@PathVariable String key, @RequestParam String app, Map<String, Object> context) {
        Map<String, Object> editorContext = Maps.newHashMap();

        Template template = templateService.findBy(EcpAppKey.valueOf(app), key);
        if (template == null) {
            log.error("template not found where app: {},template id:{}", app, key);
            throw new NotFound404Exception(messageSources.get("design.template.not.found"));
        }

        context.put(RenderConstants.EDIT_TEMPLATE, true);
        editorContext.put("template", template);

        Long pageId = 1L;
        List<Page> pages = Lists.newArrayList();
        for (TemplatePageCategory pageCategory : TemplatePageCategory.values()) {
            Page page = new Page();
            page.setId(pageId++);
            page.setName(MoreObjects.firstNonNull(messageSources.get(pageCategory.getKeyForName()),pageCategory.getDefaultName()));
            page.setTitle(MoreObjects.firstNonNull(messageSources.get(pageCategory.getKeyForName()),pageCategory.getDefaultName()));
            page.setPath(pageCategory.getPath());
            pages.add(page);
        }
        if (templateDesignHook != null) {
            List<Page> morePages = templateDesignHook.getPages();
            if (morePages != null && !morePages.isEmpty()) {
                for (Page page : morePages) {
                    page.setId(pageId++);
                    pages.add(page);
                }
            }
        }

        Map<String, Page> pageMap = Maps.newHashMap();
        for (Page page : pages) {
            pageMap.put(page.getPath(), page);
        }

        editorContext.put("pages", pageMap);
        Page indexPage = pageMap.get(TemplatePageCategory.INDEX.getPath());
        editorContext.put("indexPage", indexPage);
        editorContext.put("currentPage", indexPage);

        context.put("editorContext", editorContext);
        context.put("app", template.getApp());
        context.put("title", messageSources.get("design.template.title"));

        return configService.getDefaultFrontConfig().getRender().getEditorLayout();
    }

    @RequestMapping(value = "/{app}/render", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    public void render(@PathVariable String app, @RequestParam String key, @RequestParam(required = false) String path, HttpServletResponse response, HttpServletRequest request) {
        List<Render.Layout> layouts = configService.listLayouts(app, EcpLayoutType.SHOP.name());
        Render.Layout layout = layouts.isEmpty() ? null : layouts.get(0);
        if (layout == null) {
            throw new IllegalStateException("no default layout found for app " + app);
        }

        String templatePath = layout.getRoot() + "/" + "template";

        path = Strings.isNullOrEmpty(path) ? "index" : path;
        String templateContent = templateService.findTemplateContent(EcpAppKey.valueOf(app), key, path, false);

        Map<String, Object> context = Maps.newHashMap();
        context.put(RenderConstants.DESIGN_MODE, true);
        context.put(RenderConstants.EDIT_TEMPLATE, true);
        context.put(RenderConstants.TEMPLATE_CONTENT, templateContent);

        ThreadVars.setApp(configService.getApp(app));

        String domain = Domains.getDomainFromRequest(request);
        Site mainSite = siteService.findByDomain(domain);
        if (mainSite != null) {
            context.put(RenderConstants.MAIN_SITE, mainSite);
        }

        String pageHtml = dPageRender.naiveRender(templatePath, context);
        response.setContentType(com.google.common.net.MediaType.HTML_UTF_8.toString());
        try {
            response.getWriter().write(pageHtml);
        } catch (IOException e) {
            // ignore
        }

    }


}
