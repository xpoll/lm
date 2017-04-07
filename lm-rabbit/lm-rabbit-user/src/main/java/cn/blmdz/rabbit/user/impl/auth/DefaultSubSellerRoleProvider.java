package cn.blmdz.rabbit.user.impl.auth;

import java.util.List;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.collect.Lists;

import cn.blmdz.rabbit.common.enums.UserRole;
import cn.blmdz.rabbit.common.enums.UserType;
import cn.blmdz.rabbit.user.model.SellerRole;
import cn.blmdz.rabbit.user.model.SubSeller;
import cn.blmdz.rabbit.user.service.SellerReadService;
import cn.blmdz.rabbit.user.service.SellerRoleReadService;
import cn.blmdz.wolf.common.utils.Iters;
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
