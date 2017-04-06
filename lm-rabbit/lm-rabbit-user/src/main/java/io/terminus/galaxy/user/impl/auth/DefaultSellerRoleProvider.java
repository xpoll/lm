package io.terminus.galaxy.user.impl.auth;

import io.terminus.galaxy.common.enums.UserRole;
import io.terminus.galaxy.common.enums.UserType;
import io.terminus.galaxy.user.model.MainSellerRole;
import io.terminus.galaxy.user.model.Seller;
import io.terminus.galaxy.user.service.MainSellerRoleReadService;
import io.terminus.galaxy.user.service.SellerReadService;
import io.terminus.parana.common.utils.Iters;
import io.terminus.parana.common.utils.Strs;
import io.terminus.parana.user.auth.Role;
import io.terminus.parana.user.auth.RoleProvider;
import io.terminus.parana.user.auth.RoleProviderRegistry;
import lombok.extern.slf4j.Slf4j;
import lombok.val;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author Effet
 */
@Slf4j
@Component
public class DefaultSellerRoleProvider implements RoleProvider {

    @Autowired
    private SellerReadService sellerReadService;

    @Autowired
    private MainSellerRoleReadService mainSellerRoleReadService;

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
        val rSeller = sellerReadService.findSellerByUserId(userId);
        if (!rSeller.isSuccess()) {
            log.warn("find seller fail, userId={}, error={}", userId, rSeller.getError());
            return null;
        }
        Seller seller = rSeller.getResult().orNull();
        if (seller == null) {
            log.warn("seller not exist of userId={}", userId);
            return null;
        }
        boolean isSeller = seller.isActive();
        if (isSeller) {
            List<String> nodes = new ArrayList<>();
            Map<String, String> map = seller.getExtra();
            if (map != null) {
                Long roleId = Strs.parseLong(map.get("roleId")).orNull();
                if (roleId != null) {
                    val rRole = mainSellerRoleReadService.findById(roleId);
                    if (!rRole.isSuccess()) {
                        log.warn("find role of seller failed, roleId={}, userId={}", roleId, userId);
                    } else {
                        MainSellerRole role = rRole.getResult();
                        if (role.isActive()) {
                            nodes.addAll(Iters.nullToEmpty(role.getAllow()));
                        }
                    }
                }
            }
            return Role.createDynamic(UserRole.SELLER.name(), nodes);
        }
        return null;
    }
}
