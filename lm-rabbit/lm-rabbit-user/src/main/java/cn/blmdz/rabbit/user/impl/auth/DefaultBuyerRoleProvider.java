package cn.blmdz.rabbit.user.impl.auth;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.rabbit.common.enums.UserRole;
import cn.blmdz.rabbit.common.enums.UserType;
import cn.blmdz.wolf.user.auth.Role;
import cn.blmdz.wolf.user.auth.RoleProvider;
import cn.blmdz.wolf.user.auth.RoleProviderRegistry;
import cn.blmdz.wolf.user.model.User;
import cn.blmdz.wolf.user.service.UserReadService;
import lombok.val;
import lombok.extern.slf4j.Slf4j;

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
