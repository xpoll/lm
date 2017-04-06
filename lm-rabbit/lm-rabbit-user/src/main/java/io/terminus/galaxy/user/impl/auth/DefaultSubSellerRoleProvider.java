package io.terminus.galaxy.user.impl.auth;

import com.google.common.collect.Lists;
import io.terminus.galaxy.common.enums.UserRole;
import io.terminus.galaxy.common.enums.UserType;
import io.terminus.galaxy.user.model.SellerRole;
import io.terminus.galaxy.user.model.SubSeller;
import io.terminus.galaxy.user.service.SellerReadService;
import io.terminus.galaxy.user.service.SellerRoleReadService;
import io.terminus.parana.common.utils.Iters;
import io.terminus.parana.user.auth.Role;
import io.terminus.parana.user.auth.RoleProvider;
import io.terminus.parana.user.auth.RoleProviderRegistry;
import lombok.extern.slf4j.Slf4j;
import lombok.val;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.List;

/**
 * @author Effet
 */
@Slf4j
@Component
public class DefaultSubSellerRoleProvider implements RoleProvider {

    @Autowired
    private SellerReadService sellerReadService;

    @Autowired
    private SellerRoleReadService sellerRoleReadService;

    @Autowired
    private RoleProviderRegistry roleProviderRegistry;

    @PostConstruct
    public void init() {
        roleProviderRegistry.addRoleProvider(this);
    }

    @Override
    public int acceptType() {
        return UserType.SUB_ACCOUNT.value();
    }

    @Override
    public Role getRoleByUserId(Long userId) {
        boolean isSubSeller = false;
        Long roleId = null;
        val subResp = sellerReadService.findSubSellerByUserId(userId);
        if (!subResp.isSuccess()) {
            log.warn("sub seller find fail, userId={}, error={}", userId, subResp.getError());
        } else {
            SubSeller sub = subResp.getResult().orNull();
            if (sub != null) {
                isSubSeller = sub.isActive();
                roleId = getRoleId(sub);
            }
        }
        if (isSubSeller) {
            List<String> nodes = Lists.newArrayList();
            if (roleId != null) {
                val rRole = sellerRoleReadService.findById(roleId);
                if (!rRole.isSuccess()) {
                    log.warn("sub seller role find fail, userId={}, roleId={}, error={}", userId, roleId, rRole.getError());
                } else {
                    SellerRole role = rRole.getResult();
                    if (role.isActive()) {
                        nodes.addAll(Iters.nullToEmpty(role.getAllow()));
                    }
                }
            }
            return Role.createDynamic(UserRole.SELLER.name(), nodes);
        }
        return null;
    }

    private Long getRoleId(SubSeller sub) {
        List<SubSeller.SubSellerRole> roles = sub.getRoles();
        if (roles == null || roles.isEmpty()) {
            return null;
        }
        // 只支持赋予一个角色
        return roles.get(0).getId();
    }
}
