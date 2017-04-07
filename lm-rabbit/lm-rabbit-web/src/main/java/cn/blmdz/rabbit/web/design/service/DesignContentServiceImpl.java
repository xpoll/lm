package cn.blmdz.rabbit.web.design.service;

import static cn.blmdz.home.common.util.Arguments.isNull;
import static cn.blmdz.home.common.util.Arguments.notEmpty;
import static cn.blmdz.home.common.util.Arguments.notNull;
import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkState;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import cn.blmdz.hunt.design.medol.Page;
import cn.blmdz.hunt.design.medol.Site;
import cn.blmdz.hunt.design.service.PagePartService;
import cn.blmdz.hunt.design.service.PageService;
import cn.blmdz.hunt.design.service.SiteService;
import cn.blmdz.hunt.design.util.PagePartPath;
import cn.blmdz.hunt.design.util.PagePartScope;
import cn.blmdz.rabbit.web.design.dao.TemplateDao;
import cn.blmdz.rabbit.web.design.ext.TemplateDesignHook;
import cn.blmdz.rabbit.web.design.modal.BaseDesignContent;
import cn.blmdz.rabbit.web.design.modal.EcpAppKey;
import cn.blmdz.rabbit.web.design.modal.ShopTemplateContent;
import cn.blmdz.rabbit.web.design.modal.SiteContent;
import cn.blmdz.rabbit.web.design.modal.Template;
import cn.blmdz.rabbit.web.design.modal.TemplatePageCategory;

/**
 * Author:cp
 * Created on 11/4/15.
 */
@Service
public class DesignContentServiceImpl implements DesignContentService {

    @Autowired
    private TemplateDao templateDao;

    @Autowired
    private EcpTemplateService ecpTemplateService;

    @Autowired(required = false)
    private SiteService siteService;

    @Autowired(required = false)
    private PageService pageService;

    @Autowired(required = false)
    private PagePartService pagePartService;

    @Autowired(required = false)
    private TemplateDesignHook templateDesignHook;

    @Override
    public ShopTemplateContent exportShopTemplate(final String app, final String key) {
        Template template = templateDao.findTemplate(app, key);
        checkState(notNull(template), "template not found");

        ShopTemplateContent shopTemplateContent = new ShopTemplateContent();
        shopTemplateContent.setApp(app);
        shopTemplateContent.setTemplate(template);

        //TODO: use pipelined
        for (String path : listTemplatePath()) {
            String draftHbs = ecpTemplateService.getHbsContentDraft(EcpAppKey.valueOf(app), key, path);
            if (notEmpty(draftHbs)) {
                shopTemplateContent.setLayout(path, draftHbs);
            }
        }

        return shopTemplateContent;
    }

    @Override
    public SiteContent exportSite(Long siteId) {
        SiteContent siteContent = new SiteContent();

        Site site = siteService.findById(siteId);
        checkState(notNull(site), "site not found");
        siteContent.setApp(site.getApp());
        siteContent.setSite(site);

        // export page parts
        List<Page> pages = pageService.listBySite(siteId);
        if (notEmpty(pages)) {
            siteContent.setPages(pages);

            Map<Long, Map<String, String>> pageParts = new HashMap<>();
            for (Page page : pages) {
                Map<String, String> pagePart = pagePartService.findByKey(site.getApp(), pagePartKey(page.getId()));
                if (!CollectionUtils.isEmpty(pagePart)) {
                    pageParts.put(page.getId(), pagePart);
                }
            }
            if (!CollectionUtils.isEmpty(pageParts)) {
                siteContent.setPageParts(pageParts);
            }
        }

        //export site part
        Map<String, String> siteParts = pagePartService.findByKey(site.getApp(), sitePartKey(siteId));
        if (!CollectionUtils.isEmpty(siteParts)) {
            siteContent.setSiteParts(siteParts);
        }

        //export global part
        Map<String, String> globalParts = pagePartService.findByKey(site.getApp(), globalPartKey(site.getApp()));
        if (!CollectionUtils.isEmpty(globalParts)) {
            siteContent.setGlobalParts(globalParts);
        }

        return siteContent;
    }

    @Override
    public void importShopTemplate(ShopTemplateContent shopTemplateContent) {
        //TODO:check more strictly
        check(shopTemplateContent);
        checkArgument(notNull(shopTemplateContent.getTemplate()), "template not found");

        //导入模板时默认未发布,且非默认
        shopTemplateContent.getTemplate().setStatus(Template.Status.INIT.ordinal());
        shopTemplateContent.getTemplate().setDefault(false);

        String key = ecpTemplateService.create(shopTemplateContent.getTemplate());
        EcpAppKey appKey = EcpAppKey.valueOf(shopTemplateContent.getApp());

        for (String path : listTemplatePath()) {
            String draftHbs = shopTemplateContent.getLayoutHbsPair().get(path);
            if (notEmpty(draftHbs)) {
                ecpTemplateService.save(appKey, key, path, draftHbs);
            }
        }
    }

    @Override
    public void importSite(SiteContent siteContent) {
        check(siteContent);
        checkArgument(notNull(siteContent.getSite()), "site not found");

        Site existed = siteService.findByDomain(siteContent.getSite().getDomain());
        checkState(isNull(existed), "site.existed");

        Long siteId = siteService.create(siteContent.getSite(), false);

        if (notEmpty(siteContent.getPages())) {
            for (Page page : siteContent.getPages()) {
                Map<String, String> pageParts = siteContent.getPageParts().get(page.getId());
                Long pageId = pageService.create(siteId, page);
                if (!CollectionUtils.isEmpty(pageParts)) {
                    pagePartService.put(siteContent.getApp(), pagePartKey(pageId), pageParts);
                }
            }
        }

        if (!CollectionUtils.isEmpty(siteContent.getSiteParts())) {
            pagePartService.put(siteContent.getApp(), sitePartKey(siteId), siteContent.getSiteParts());
        }

        if (!CollectionUtils.isEmpty(siteContent.getGlobalParts())) {
            pagePartService.put(siteContent.getApp(), globalPartKey(siteContent.getApp()), siteContent.getGlobalParts());
        }
    }

    private String globalPartKey(String app) {
        return "app:" + app + ":page-part:" + "[GLOBAL]";
    }

    private String sitePartKey(Long siteId) {
        return new PagePartPath(PagePartScope.SITE.toString(), siteId, null).toString();
    }

    private String pagePartKey(Long pageId) {
        return new PagePartPath(PagePartScope.PAGE.toString(), pageId, null).toString();
    }

    private void check(BaseDesignContent baseDesignContent) {
        checkArgument(notNull(baseDesignContent), "design content can not be null");
        checkArgument(notEmpty(baseDesignContent.getApp()), "app can not be null");
    }

    private Set<String> listTemplatePath() {
        Set<String> templatePaths = new HashSet<>();
        for (TemplatePageCategory templatePageCategory : TemplatePageCategory.values()) {
            templatePaths.add(templatePageCategory.getPath());
        }
        if (templateDesignHook != null) {
            List<Page> morePages = templateDesignHook.getPages();
            if (morePages != null && !morePages.isEmpty()) {
                for (Page page : morePages) {
                    templatePaths.add(page.getPath());
                }
            }
        }
        return templatePaths;
    }

}
