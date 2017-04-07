package cn.blmdz.rabbit.web.core.auth;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Objects;

import cn.blmdz.rabbit.common.enums.UserType;
import cn.blmdz.wolf.auth.core.AclLoader;
import cn.blmdz.wolf.auth.core.AuthenticationProperties;
import cn.blmdz.wolf.auth.core.DefaultAuthenticator;
import cn.blmdz.wolf.auth.core.PermissionHelper;
import cn.blmdz.wolf.auth.model.Req;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.user.model.User;
import cn.blmdz.wolf.user.service.UserReadService;
import cn.blmdz.wolf.web.core.util.ParanaUserMaker;
import lombok.val;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Effet
 */
@Slf4j
@Component("authenticator")
public class GalaxyAuthenticator extends DefaultAuthenticator {

    private final UserReadService<User> userReadService;

    @Autowired
    public GalaxyAuthenticator(AclLoader aclLoader,
            PermissionHelper permissionHelper, AuthenticationProperties authenticationProperties, UserReadService<User> userReadService) {
        super(aclLoader, permissionHelper, authenticationProperties.getLevel());
        this.userReadService = userReadService;
    }

    @Override
    public boolean ask(ParanaUser user, Req req) {
        if (!super.ask(user, req)) {
            return false;
        }
        if (user == null || !Objects.equal(user.getType(), UserType.SUB_ACCOUNT.value())) {
            return true;
        }
        ParanaUser main = findMainSeller(user);
        if (main == null) {
            return false;
        }
        return super.ask(main, req);
    }

    @Override
    public boolean ask(ParanaUser user, String key) {
        if (!super.ask(user, key)) {
            return false;
        }
        if (user == null || !Objects.equal(user.getType(), UserType.SUB_ACCOUNT.value())) {
            return true;
        }
        ParanaUser main = findMainSeller(user);
        if (main == null) {
            return false;
        }
        return super.ask(main, key);
    }

    private ParanaUser findMainSeller(ParanaUser sub) {
        Long userId = sub.getPresentUserId();
        if (Objects.equal(userId, sub.getId())) {
            return null;
        }
        val rUser = userReadService.findById(userId);
        if (!rUser.isSuccess()) {
            return null;
        }
        User user = rUser.getResult();
        if (!Objects.equal(user.getType(), UserType.NORMAL.value())) {
            return null;
        }
        return ParanaUserMaker.from(user);
    }
}
