package cn.blmdz.rabbit.user.impl.auth;

import java.util.List;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.collect.Lists;

import cn.blmdz.rabbit.common.enums.UserRole;
import cn.blmdz.rabbit.common.enums.UserType;
import cn.blmdz.rabbit.user.service.OperatorReadService;
import cn.blmdz.rabbit.user.service.OperatorRoleReadService;
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
public class DefaultOperatorRoleProvider implements RoleProvider {

    @Autowired
    private OperatorReadService operatorReadService;

    @Autowired
    private OperatorRoleReadService operatorRoleReadService;

    @Autowired
    private RoleProviderRegistry roleProviderRegistry;

    @PostConstruct
    public void init() {
        roleProviderRegistry.addRoleProvider(this);
    }

    @Override
    public int acceptType() {
        return UserType.OPERATOR.value();
    }

    @Override
    public Role getRoleByUserId(Long userId) {
        val resp = operatorReadService.findByUserId(userId);
        if (!resp.isSuccess()) {
            log.warn("operator find fail, userId={}, error={}", userId, resp.getError());
            return null;
        }
        Long roleId = resp.getResult().getRoleId();
        List<String> nodes = Lists.newArrayList();
        if (roleId != null) {
            val roleResp = operatorRoleReadService.findById(roleId);
            if (!roleResp.isSuccess()) {
                log.warn("find role(id={}) of operator(userId={}) failed, error={}", roleId, userId, roleResp.getError());
            } else {
                nodes.addAll(Iters.nullToEmpty(roleResp.getResult().getAllow()));
            }
        }
        return Role.createDynamic(UserRole.ADMIN.name(), Iters.nullToEmpty(nodes));
    }
}
