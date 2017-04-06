package io.terminus.galaxy.user.impl.auth;

import io.terminus.galaxy.common.enums.UserRole;
import io.terminus.galaxy.common.enums.UserType;
import io.terminus.parana.user.auth.Role;
import io.terminus.parana.user.auth.RoleProvider;
import io.terminus.parana.user.auth.RoleProviderRegistry;
import io.terminus.parana.user.model.User;
import io.terminus.parana.user.service.UserReadService;
import lombok.extern.slf4j.Slf4j;
import lombok.val;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

/**
 * @author Effet
 */
@Slf4j
@Component
public class DefaultBuyerRoleProvider implements RoleProvider {

    @Autowired
    private UserReadService<User> userReadService;

    @Autowired
    private RoleProviderRegistry roleProviderRegistry;

    @PostConstruct
    public void init() {
        roleProviderRegistry.addRoleProvider(this);
    }

    @Override
    public int acceptType() {
        return UserType.NORMAL.value();
    }

    @Override
    public Role getRoleByUserId(Long userId) {
        val userResp = userReadService.findById(userId);
        if (!userResp.isSuccess()) {
            log.warn("find user failed, userId={}, error={}", userId, userResp.getError());
            return null;
        }
        User user = userResp.getResult();
        boolean isBuyer = false;
        if (user.getRoles() != null) {
            for (String role : user.getRoles()) {
                if (role.equalsIgnoreCase(UserRole.BUYER.name())) {
                    isBuyer = true;
                    break;
                }
            }
        }
        if (isBuyer) {
            return Role.createStatic(UserRole.BUYER.name());
        }
        return null;
    }
}
