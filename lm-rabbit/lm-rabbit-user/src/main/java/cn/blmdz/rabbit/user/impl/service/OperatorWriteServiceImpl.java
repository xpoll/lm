package cn.blmdz.rabbit.user.impl.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Params;
import cn.blmdz.rabbit.common.enums.UserRole;
import cn.blmdz.rabbit.common.enums.UserStatus;
import cn.blmdz.rabbit.common.enums.UserType;
import cn.blmdz.rabbit.user.impl.dao.OperatorDao;
import cn.blmdz.rabbit.user.impl.dao.OperatorRoleDao;
import cn.blmdz.rabbit.user.impl.manager.GalaxyUserManager;
import cn.blmdz.rabbit.user.model.Operator;
import cn.blmdz.rabbit.user.model.OperatorRole;
import cn.blmdz.rabbit.user.service.OperatorWriteService;
import cn.blmdz.wolf.common.utils.EncryptUtil;
import cn.blmdz.wolf.user.model.User;
import lombok.extern.slf4j.Slf4j;

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
