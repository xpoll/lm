package cn.blmdz.rabbit.user.impl.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.user.impl.dao.SellerRoleDao;
import cn.blmdz.rabbit.user.model.SellerRole;
import cn.blmdz.rabbit.user.service.SellerRoleWriteService;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Effet
 */
@Slf4j
@Service
public class SellerRoleWriteServiceImpl implements SellerRoleWriteService {

    private final SellerRoleDao sellerRoleDao;

    @Autowired
    public SellerRoleWriteServiceImpl(SellerRoleDao sellerRoleDao) {
        this.sellerRoleDao = sellerRoleDao;
    }

    @Override
    public Response<Long> createRole(SellerRole sellerRole) {
        try {
            sellerRoleDao.create(sellerRole);
            return Response.ok(sellerRole.getId());
        } catch (Exception e) {
            log.error("create role failed, sellerRole={}, cause:{}",
                    sellerRole, Throwables.getStackTraceAsString(e));
            return Response.fail("seller.role.create.fail");
        }
    }

    @Override
    public Response<Boolean> updateRole(SellerRole sellerRole) {
        try {
            return Response.ok(sellerRoleDao.update(sellerRole));
        } catch (Exception e) {
            log.error("update role failed, sellerRole={}, cause:{}",
                    sellerRole, Throwables.getStackTraceAsString(e));
            return Response.fail("seller.role.update.fail");
        }
    }
}
