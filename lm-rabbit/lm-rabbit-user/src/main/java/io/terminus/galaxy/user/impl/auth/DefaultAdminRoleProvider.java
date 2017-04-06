package io.terminus.galaxy.user.impl.auth;

import io.terminus.galaxy.common.enums.UserRole;
import io.terminus.galaxy.common.enums.UserType;
import io.terminus.parana.user.auth.Role;
import io.terminus.parana.user.auth.RoleProvider;
import io.terminus.parana.user.auth.RoleProviderRegistry;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

/**
 * @author Effet
 */
@Component
public class DefaultAdminRoleProvider implements RoleProvider {

    @Autowired
    private RoleProviderRegistry roleProviderRegistry;

    @PostConstruct
    public void init() {
        roleProviderRegistry.addRoleProvider(this);
    }

    @Override
    public int acceptType() {
        return UserType.ADMIN.value();
    }

    @Override
    public Role getRoleByUserId(Long userId) {
        return Role.createStatic(UserRole.ADMIN.name());
    }
}
