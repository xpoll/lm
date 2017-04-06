package io.terminus.galaxy.user.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.galaxy.user.impl.dao.OperatorRoleDao;
import io.terminus.galaxy.user.model.OperatorRole;
import io.terminus.galaxy.user.service.OperatorRoleWriteService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author Effet
 */
@Slf4j
@Service
public class OperatorRoleWriteServiceImpl implements OperatorRoleWriteService {

    private final OperatorRoleDao operatorRoleDao;

    @Autowired
    public OperatorRoleWriteServiceImpl(OperatorRoleDao operatorRoleDao) {
        this.operatorRoleDao = operatorRoleDao;
    }

    @Override
    public Response<Long> createRole(OperatorRole operatorRole) {
        try {
            operatorRoleDao.create(operatorRole);
            return Response.ok(operatorRole.getId());
        } catch (Exception e) {
            log.error("create role failed, operatorRole={}, cause:{}",
                    operatorRole, Throwables.getStackTraceAsString(e));
            return Response.fail("operator.role.create.fail");
        }
    }

    @Override
    public Response<Boolean> updateRole(OperatorRole operatorRole) {
        try {
            return Response.ok(operatorRoleDao.update(operatorRole));
        } catch (Exception e) {
            log.error("update role failed, operatorRole={}, cause:{}",
                    operatorRole, Throwables.getStackTraceAsString(e));
            return Response.fail("operator.role.update.fail");
        }
    }
}
