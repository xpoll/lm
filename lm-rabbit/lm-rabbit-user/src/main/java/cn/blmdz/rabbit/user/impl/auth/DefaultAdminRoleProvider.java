package cn.blmdz.rabbit.user.impl.auth;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.rabbit.common.enums.UserRole;
import cn.blmdz.rabbit.common.enums.UserType;
import cn.blmdz.wolf.user.auth.Role;
import cn.blmdz.wolf.user.auth.RoleProvider;
import cn.blmdz.wolf.user.auth.RoleProviderRegistry;

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
