package io.terminus.galaxy.user.impl.service;

import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Params;
import io.terminus.galaxy.common.enums.UserRole;
import io.terminus.galaxy.common.enums.UserType;
import io.terminus.galaxy.user.impl.dao.OperatorDao;
import io.terminus.galaxy.user.impl.dao.OperatorRoleDao;
import io.terminus.galaxy.user.impl.manager.GalaxyUserManager;
import io.terminus.galaxy.user.model.Operator;
import io.terminus.galaxy.user.model.OperatorRole;
import io.terminus.galaxy.user.service.OperatorWriteService;
import io.terminus.parana.common.enums.UserStatus;
import io.terminus.parana.common.utils.EncryptUtil;
import io.terminus.parana.user.model.User;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author Effet
 */
@Slf4j
@Service
public class OperatorWriteServiceImpl implements OperatorWriteService {

    private final OperatorDao operatorDao;

    private final OperatorRoleDao operatorRoleDao;

    private final GalaxyUserManager galaxyUserManager;

    @Autowired
    public OperatorWriteServiceImpl(OperatorDao operatorDao, OperatorRoleDao operatorRoleDao, GalaxyUserManager galaxyUserManager) {
        this.operatorDao = operatorDao;
        this.operatorRoleDao = operatorRoleDao;
        this.galaxyUserManager = galaxyUserManager;
    }

    @Override
    public Response<Long> create(User user, Operator operator) {
        try {
            String un = Params.trimToNull(user.getName());
            if (un == null) {
                log.warn("create operator failed, no username specified");
                return Response.fail("operator.create.fail.no.username");
            }
            String pw = Params.trimToNull(user.getPassword());
            if (pw == null) {
                log.warn("create operator failed, no password specified");
                return Response.fail("operator.create.fail.no.password");
            }

            User u = new User();
            u.setName(un);
            u.setPassword(EncryptUtil.encrypt(pw));
            u.setType(UserType.OPERATOR.value());
            u.setRoles(Lists.newArrayList(UserRole.ADMIN.name()));
            u.setStatus(UserStatus.NORMAL.value());

            operator.setUserName(un);
            operator.setStatus(1);
            Long roleId = operator.getRoleId();
            operator.setRoleName(findRoleName(roleId));
            Long userId = galaxyUserManager.createOperator(u, operator);
            return Response.ok(userId);
        } catch (Exception e) {
            log.error("create operator failed, operator={}, cause:{}",
                    operator, Throwables.getStackTraceAsString(e));
            return Response.fail("operator.create.fail");
        }
    }

    @Override
    public Response<Boolean> update(Operator operator) {
        try {
            return Response.ok(operatorDao.update(operator));
        } catch (Exception e) {
            log.error("update operator failed, operator={}, cause:{}",
                    operator, Throwables.getStackTraceAsString(e));
            return Response.fail("operator.update.fail");
        }
    }

    private String findRoleName(Long roleId) {
        if (roleId == null) {
            return "";
        }
        OperatorRole r = operatorRoleDao.findById(roleId);
        if (r == null) {
            log.warn("role not found, id={}", roleId);
            return "";
        }
        return Strings.nullToEmpty(r.getName());
    }
}
