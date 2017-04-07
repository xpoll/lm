package cn.blmdz.rabbit.user.impl.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.PageInfo;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.user.impl.dao.SellerRoleDao;
import cn.blmdz.rabbit.user.model.SellerRole;
import cn.blmdz.rabbit.user.service.SellerRoleReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Effet
 */
@Slf4j
@Service
public class SellerRoleReadServiceImpl implements SellerRoleReadService {

    private final SellerRoleDao sellerRoleDao;

    @Autowired
    public SellerRoleReadServiceImpl(SellerRoleDao sellerRoleDao) {
        this.sellerRoleDao = sellerRoleDao;
    }

    @Override
    public Response<SellerRole> findById(Long id) {
        try {
            return Response.ok(sellerRoleDao.findById(id));
        } catch (Exception e) {
            log.error("find seller role by id={} failed, cause:{}",
                    id, Throwables.getStackTraceAsString(e));
            return Response.fail("seller.role.find.fail");
        }
    }

    @Override
    public Response<List<SellerRole>> findByIds(List<Long> ids) {
        try {
            return Response.ok(sellerRoleDao.findByIds(ids));
        } catch (Exception e) {
            log.error("find seller role by ids={} failed, cause:{}",
                    ids, Throwables.getStackTraceAsString(e));
            return Response.fail("seller.role.find.fail");
        }
    }

    @Override
    public Response<List<SellerRole>> findByShopIdAndStatus(String appKey, Long shopId, Integer status) {
        try {
            return Response.ok(sellerRoleDao.findByShopIdAndStatus(appKey, shopId, status));
        } catch (Exception e) {
            log.error("find seller roles by shopId={} failed, cause:{}",
                    shopId, Throwables.getStackTraceAsString(e));
            return Response.fail("seller.role.find.fail");
        }
    }

    @Override
    public Response<Paging<SellerRole>> pagination(String appKey, Long shopId, Integer status, String name, Integer pageNo, Integer size) {
        try {
            PageInfo page = new PageInfo(pageNo, size);
            SellerRole criteria = new SellerRole();
            criteria.setAppKey(appKey);
            criteria.setShopId(shopId);
            criteria.setStatus(status);
            criteria.setName(name);
            return Response.ok(sellerRoleDao.paging(page.getOffset(), page.getLimit(), criteria));
        } catch (Exception e) {
            log.error("paging seller roles failed, shopId={}, status={}, pageNo={}, size={}, cause:{}",
                    shopId, status, pageNo, size, Throwables.getStackTraceAsString(e));
            return Response.fail("seller.role.paging.fail");
        }
    }
}
