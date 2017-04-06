package io.terminus.galaxy.user.impl.auth;

import com.google.common.collect.Lists;
import io.terminus.galaxy.common.enums.UserRole;
import io.terminus.galaxy.common.enums.UserType;
import io.terminus.galaxy.user.service.OperatorReadService;
import io.terminus.galaxy.user.service.OperatorRoleReadService;
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
