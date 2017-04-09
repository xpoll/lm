package cn.blmdz.rabbit.web.design.controller.ext;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.hunt.design.controller.ext.DesignHook;
import cn.blmdz.hunt.design.medol.Site;
import cn.blmdz.hunt.design.service.SiteService;
import cn.blmdz.rabbit.web.design.service.EcpSiteService;
import cn.blmdz.wolf.shop.service.ShopReadService;

/**
 * Created by IntelliJ IDEA.
 * User: AnsonChan
 * Date: 15/1/15
 */
@Component
public class EcpDesignHookImpl implements DesignHook {
    @Autowired(required = false)
    private SiteService siteService;
    @Autowired(required = false)
    private ShopReadService shopReadService;
    @Autowired
    private EcpSiteService ecpSiteService;

    @Override
    public void design(Site site, Map<String, Object> context) {
        authCheck(site);
    }

    @Override
    public void page(Long pageId, Map<String, Object> context) {
        Site site = siteService.findByPageId(pageId);
        authCheck(site);
        boolean isShopLayout = ecpSiteService.isShopLayout(site);
        if (isShopLayout) {
            context.put("sellerId", site.getUserId());
            context.put("shopId", shopReadService.findByUserId(site.getUserId()).getResult().getId());
        }
    }

    @Override
    public String customPage(String url, String pagePath) {
//        BaseUser BaseUser = UserUtil.getCurrentUser();
//        User.TYPE userType = User.TYPE.fromNumber(BaseUser.getType());
//        if (userType == User.TYPE.ADMIN) {
//            return null;
//        }
//        throw new JsonResponseException(401, "design.no.auth");
        return null;
    }

    private void authCheck(Site site) {
//        BaseUser BaseUser = UserUtil.getCurrentUser();
//        User.TYPE userType = User.TYPE.fromNumber(BaseUser.getType());
//        if (userType == User.TYPE.ADMIN) {
//            return;
//        }
//        if (Objects.equal(site.getUserId(), BaseUser.getId())) {
//            return;
//        }
//        throw new JsonResponseException(401, "design.no.auth");
    }
}
