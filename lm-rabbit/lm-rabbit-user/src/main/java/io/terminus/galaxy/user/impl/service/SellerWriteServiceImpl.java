package io.terminus.galaxy.user.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.galaxy.user.impl.dao.SellerDao;
import io.terminus.galaxy.user.impl.dao.SubSellerDao;
import io.terminus.galaxy.user.model.Seller;
import io.terminus.galaxy.user.model.SubSeller;
import io.terminus.galaxy.user.service.SellerWriteService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author Effet
 */
@Slf4j
@Service
public class SellerWriteServiceImpl implements SellerWriteService {

    private final SellerDao sellerDao;

    private final SubSellerDao subSellerDao;

    @Autowired
    public SellerWriteServiceImpl(SellerDao sellerDao, SubSellerDao subSellerDao) {
        this.sellerDao = sellerDao;
        this.subSellerDao = subSellerDao;
    }

    @Override
    public Response<Long> createSeller(Seller seller) {
        try {
            sellerDao.create(seller);
            return Response.ok(seller.getId());
        } catch (Exception e) {
            log.error("create seller failed, seller={}, cause:{}",
                    seller, Throwables.getStackTraceAsString(e));
            return Response.fail("seller.create.fail");
        }
    }

    @Override
    public Response<Boolean> updateSeller(Seller seller) {
        try {
            return Response.ok(sellerDao.update(seller));
        } catch (Exception e) {
            log.error("update seller failed, seller={}, cause:{}",
                    seller, Throwables.getStackTraceAsString(e));
            return Response.fail("seller.update.fail");
        }
    }

    @Override
    public Response<Long> createSubSeller(SubSeller subSeller) {
        try {
            subSellerDao.create(subSeller);
            return Response.ok(subSeller.getId());
        } catch (Exception e) {
            log.error("create sub seller={} failed, cause:{}",
                    subSeller, Throwables.getStackTraceAsString(e));
            return Response.fail("seller.sub.create.fail");
        }
    }

    @Override
    public Response<Boolean> updateSubSeller(SubSeller subSeller) {
        try {
            return Response.ok(subSellerDao.update(subSeller));
        } catch (Exception e) {
            log.error("update sub seller={} failed, cause:{}",
                    subSeller, Throwables.getStackTraceAsString(e));
            return Response.fail("seller.sub.update.fail");
        }
    }
}
