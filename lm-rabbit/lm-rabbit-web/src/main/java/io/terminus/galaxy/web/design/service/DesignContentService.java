package io.terminus.galaxy.web.design.service;


import io.terminus.galaxy.web.design.modal.ShopTemplateContent;
import io.terminus.galaxy.web.design.modal.SiteContent;

/**
 * Author:cp
 * Created on 11/4/15.
 */
public interface DesignContentService {
    ShopTemplateContent exportShopTemplate(String app, String key);

    SiteContent exportSite(Long siteId);

    void importShopTemplate(ShopTemplateContent shopTemplateContent);

    void importSite(SiteContent siteContent);
}
