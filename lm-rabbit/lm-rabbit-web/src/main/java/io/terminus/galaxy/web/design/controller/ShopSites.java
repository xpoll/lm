package io.terminus.galaxy.web.design.controller;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.BaseUser;
import io.terminus.pampas.common.UserUtil;
import io.terminus.pampas.design.model.Site;
import io.terminus.pampas.design.service.SiteService;
import io.terminus.pampas.engine.exception.UnAuthorize401Exception;
import io.terminus.galaxy.web.design.modal.EcpAppKey;
import io.terminus.galaxy.web.design.service.EcpSiteService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * Created by IntelliJ IDEA.
 * User: AnsonChan
 * Date: 15/1/15
 */
@RequestMapping("/api/design/shop-sites")
@Controller
@Slf4j
public class ShopSites {
    @Autowired(required = false)
    private SiteService siteService;
    @Autowired
    private EcpSiteService ecpSiteService;

    @RequestMapping(value = "/init", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public void init(@RequestParam(required = false) String app, @RequestParam(required = false) String layout) {
        BaseUser BaseUser = UserUtil.getCurrentUser();
//        if (User.TYPE.fromNumber(BaseUser.getType()) != User.TYPE.SELLER && User.TYPE.fromNumber(BaseUser.getType()) != User.TYPE.SITE_OWNER) {
//            throw new UnAuthorize401Exception("only seller can init self shop sites");
//        }
        if (Strings.isNullOrEmpty(layout)) {
            ecpSiteService.initShopSite(BaseUser.getId());
        } else {
            ecpSiteService.createShopSite(BaseUser.getId(), EcpAppKey.valueOf(app), layout);
        }
    }

    @RequestMapping(value = "/init/{sellerId}", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public void init(@PathVariable Long sellerId,
                     @RequestParam(required = false) String app, @RequestParam(required = false) String layout) {
        BaseUser BaseUser = UserUtil.getCurrentUser();
//        if (User.TYPE.fromNumber(BaseUser.getType()) != User.TYPE.ADMIN) {
//            throw new UnAuthorize401Exception("only seller can init self shop sites");
//        }
        if (Strings.isNullOrEmpty(layout)) {
            ecpSiteService.initShopSite(sellerId);
        } else {
            ecpSiteService.createShopSite(sellerId, EcpAppKey.valueOf(app), layout);
        }
    }

    @RequestMapping(value = "/{siteId}/active", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public void active(@PathVariable Long siteId) {
        BaseUser BaseUser = UserUtil.getCurrentUser();
//        if (User.TYPE.fromNumber(BaseUser.getType()) != User.TYPE.SELLER && User.TYPE.fromNumber(BaseUser.getType()) != User.TYPE.SITE_OWNER) {
//            throw new UnAuthorize401Exception("only seller can init self shop sites");
//        }
        Site site = siteService.findById(siteId);
        if (site == null) {
            log.warn("site [{}] not found when set active", siteId);
            throw new JsonResponseException(400, "site not found");
        }
        if (!Objects.equal(site.getUserId(), BaseUser.getId())) {
            throw new UnAuthorize401Exception("site not belong to you");
        }
        ecpSiteService.setCurrentShopSiteByMode(BaseUser.getId(), site);
    }
}
