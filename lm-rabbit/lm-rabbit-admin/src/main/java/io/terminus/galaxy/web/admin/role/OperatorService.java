package io.terminus.galaxy.web.admin.role;

import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.BaseUser;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.galaxy.common.enums.UserType;
import io.terminus.galaxy.user.model.Operator;
import io.terminus.galaxy.user.model.OperatorRole;
import io.terminus.galaxy.user.service.OperatorReadService;
import io.terminus.galaxy.user.service.OperatorRoleReadService;
import io.terminus.pampas.client.Export;
import io.terminus.parana.common.utils.RespHelper;
import io.terminus.parana.user.model.User;
import io.terminus.parana.user.service.UserReadService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * @author Effet
 */
@Slf4j
@Component
public class OperatorService {

    private final UserReadService<User> userReadService;

    private final OperatorReadService operatorReadService;

    private final OperatorRoleReadService operatorRoleReadService;

    @Autowired
    public OperatorService(UserReadService<User> userReadService,
                           OperatorReadService operatorReadService,
                           OperatorRoleReadService operatorRoleReadService) {
        this.userReadService = userReadService;
        this.operatorReadService = operatorReadService;
        this.operatorRoleReadService = operatorRoleReadService;
    }

    @Export(paramNames = {"user", "roleId", "pageNo", "pageSize"})
    public Response<Paging<Operator>> pagingOperator(BaseUser user, Long roleId, Integer pageNo, Integer pageSize) {
        try {
            Long userId = getLoginAdminId(user);

            Paging<Operator> paging = RespHelper.orServEx(operatorReadService.pagination(roleId, null, pageNo, pageSize));
            List<Operator> result = new ArrayList<>();
            List<Long> userIds = Lists.newArrayList();
            List<Long> roleIds = Lists.newArrayList();
            for (Operator operator : paging.getData()) {
                userIds.add(operator.getUserId());
                if (operator.getRoleId() != null) {
                    roleIds.add(operator.getRoleId());
                }
            }
            Map<Long, User> userMap = Maps.newHashMap();
            Response<List<User>> userResp = userReadService.findByIds(userIds);
            if (userResp.isSuccess()) {
                for (User u : userResp.getResult()) {
                    userMap.put(u.getId(), u);
                }
            }
            Map<Long, OperatorRole> roleMap = Maps.newHashMap();
            Response<List<OperatorRole>> roleResp = operatorRoleReadService.findByIds(roleIds);
            if (roleResp.isSuccess()) {
                for (OperatorRole role : roleResp.getResult()) {
                    roleMap.put(role.getId(), role);
                }
            }
            for (Operator operator : paging.getData()) {
                Operator op = new Operator();
                BeanUtils.copyProperties(operator, op);
                User u = userMap.get(operator.getUserId());
                if (u != null) {
                    op.setUserName(u.getName());
                }
                if (operator.getRoleId() != null) {
                    OperatorRole role = roleMap.get(operator.getRoleId());
                    if (role != null) {
                        op.setRoleName(role.getName());
                    }
                }
                result.add(op);
            }
            return Response.ok(new Paging<>(paging.getTotal(), result));
        } catch (ServiceException e) {
            log.warn("paging operator failed, user={}, pageNo={}, pageSize={}, error={}",
                    user, pageNo, pageSize, e.getMessage());
            return Response.fail("operator.paging.fail");
        } catch (Exception e) {
            log.error("paging operator failed, user={}, pageNo={}, pageSize={}, cause:{}",
                    user, pageNo, pageSize, Throwables.getStackTraceAsString(e));
            return Response.fail("operator.paging.fail");
        }
    }

    private Long getLoginAdminId(BaseUser user) {
        if (user == null) {
            throw new ServiceException("user.not.login");
        }
        if (!Objects.equals(user.getType(), UserType.ADMIN.value())) {
            throw new ServiceException("user.no.permission");
        }
        return user.getId();
    }
}
