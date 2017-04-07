package cn.blmdz.rabbit.user.impl.auth;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.wolf.user.auth.DefaultUserRoleLoader;
import cn.blmdz.wolf.user.auth.RoleProviderRegistry;
import cn.blmdz.wolf.user.model.User;
import cn.blmdz.wolf.user.service.AdminUserService;
import cn.blmdz.wolf.user.service.UserReadService;
import lombok.extern.slf4j.Slf4j;

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
