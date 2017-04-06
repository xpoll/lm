package io.terminus.galaxy.user.impl.auth;

import io.terminus.parana.user.auth.DefaultUserRoleLoader;
import io.terminus.parana.user.auth.RoleProviderRegistry;
import io.terminus.parana.user.model.User;
import io.terminus.parana.user.service.AdminUserService;
import io.terminus.parana.user.service.UserReadService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * @author Effet
 */
@Slf4j
@Component
public class GalaxyUserRoleLoaderImpl extends DefaultUserRoleLoader {

    @Autowired
    public GalaxyUserRoleLoaderImpl(UserReadService<User> userReadService, AdminUserService adminUserService, RoleProviderRegistry roleProviderRegistry) {
        super(userReadService, adminUserService, roleProviderRegistry);
    }
}
