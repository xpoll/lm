package cn.blmdz.rabbit.user.impl.auth;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.rabbit.common.enums.UserRole;
import cn.blmdz.rabbit.common.enums.UserType;
import cn.blmdz.rabbit.user.model.MainSellerRole;
import cn.blmdz.rabbit.user.model.Seller;
import cn.blmdz.rabbit.user.service.MainSellerRoleReadService;
import cn.blmdz.rabbit.user.service.SellerReadService;
import cn.blmdz.wolf.common.utils.Iters;
import cn.blmdz.wolf.common.utils.Strs;
import cn.blmdz.wolf.user.auth.Role;
import cn.blmdz.wolf.user.auth.RoleProvider;
import cn.blmdz.wolf.user.auth.RoleProviderRegistry;
import lombok.val;
import lombok.extern.slf4j.Slf4j;

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
