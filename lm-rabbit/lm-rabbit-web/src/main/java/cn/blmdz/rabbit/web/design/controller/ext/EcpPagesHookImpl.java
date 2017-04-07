package cn.blmdz.rabbit.web.design.controller.ext;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.hunt.design.controller.ext.PagesHook;
import cn.blmdz.hunt.design.medol.Page;
import cn.blmdz.hunt.design.service.SiteService;

/**
 * Created by IntelliJ IDEA.
 * User: AnsonChan
 * Date: 15/1/16
 */
@Component
public class EcpPagesHookImpl implements PagesHook {
    @Autowired(required = false)
    private SiteService siteService;

    @Override
    public void create(Page page) {
        authCheck(page.getSiteId());
    }

    @Override
    public void update(Page page) {
        authCheck(page.getSiteId());
    }

    @Override
    public void delete(Page page) {
        authCheck(page.getSiteId());
    }

    private void authCheck(Long siteId) {
//        Site site = siteService.findById(siteId);
//        BaseUser loginUser = UserUtil.getCurrentUser();
//        User.TYPE userType = User.TYPE.fromNumber(loginUser.getType());
//        if (userType == User.TYPE.ADMIN) {
//            return;
//        }
//        if (Objects.equal(site.getUserId(), loginUser.getId())) {
//            return;
//        }
//        throw new JsonResponseException(401, "design.no.auth");
    }
}
