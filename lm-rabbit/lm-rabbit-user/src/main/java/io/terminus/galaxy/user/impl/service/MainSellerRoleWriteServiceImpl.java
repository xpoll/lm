package io.terminus.galaxy.user.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.galaxy.user.impl.dao.MainSellerRoleDao;
import io.terminus.galaxy.user.model.MainSellerRole;
import io.terminus.galaxy.user.service.MainSellerRoleWriteService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author Effet
 */
@Slf4j
@Service
public class MainSellerRoleWriteServiceImpl implements MainSellerRoleWriteService {

    private final MainSellerRoleDao mainSellerRoleDao;

    @Autowired
    public MainSellerRoleWriteServiceImpl(MainSellerRoleDao mainSellerRoleDao) {
        this.mainSellerRoleDao = mainSellerRoleDao;
    }

    @Override
    public Response<Long> createRole(MainSellerRole mainSellerRole) {
        try {
            mainSellerRoleDao.create(mainSellerRole);
            return Response.ok(mainSellerRole.getId());
        } catch (Exception e) {
            log.error("create role failed, mainSellerRole={}, cause:{}",
                    mainSellerRole, Throwables.getStackTraceAsString(e));
            return Response.fail("operator.role.create.fail");
        }
    }

    @Override
    public Response<Boolean> updateRole(MainSellerRole mainSellerRole) {
        try {
            return Response.ok(mainSellerRoleDao.update(mainSellerRole));
        } catch (Exception e) {
            log.error("update role failed, mainSellerRole={}, cause:{}",
                    mainSellerRole, Throwables.getStackTraceAsString(e));
            return Response.fail("operator.role.update.fail");
        }
    }
}
