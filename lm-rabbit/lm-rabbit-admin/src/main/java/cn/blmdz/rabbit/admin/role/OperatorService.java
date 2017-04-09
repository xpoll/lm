package cn.blmdz.rabbit.admin.role;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.rabbit.common.enums.UserType;
import cn.blmdz.rabbit.user.model.Operator;
import cn.blmdz.rabbit.user.model.OperatorRole;
import cn.blmdz.rabbit.user.service.OperatorReadService;
import cn.blmdz.rabbit.user.service.OperatorRoleReadService;
import cn.blmdz.wolf.common.utils.RespHelper;
import cn.blmdz.wolf.user.model.User;
import cn.blmdz.wolf.user.service.UserReadService;
import lombok.extern.slf4j.Slf4j;

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

            Paging<Operator> paging = RespHelper.<Paging<Operator>>orServEx(operatorReadService.pagination(roleId, null, pageNo, pageSize));
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
