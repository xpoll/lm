package io.terminus.galaxy.web.core.auth;

import com.google.common.base.Objects;
import io.terminus.galaxy.common.enums.UserType;
import io.terminus.parana.auth.core.AclLoader;
import io.terminus.parana.auth.core.AuthenticationProperties;
import io.terminus.parana.auth.core.DefaultAuthenticator;
import io.terminus.parana.auth.core.PermissionHelper;
import io.terminus.parana.auth.model.Req;
import io.terminus.parana.common.model.ParanaUser;
import io.terminus.parana.user.model.User;
import io.terminus.parana.user.service.UserReadService;
import io.terminus.parana.web.core.util.ParanaUserMaker;
import lombok.extern.slf4j.Slf4j;
import lombok.val;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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
