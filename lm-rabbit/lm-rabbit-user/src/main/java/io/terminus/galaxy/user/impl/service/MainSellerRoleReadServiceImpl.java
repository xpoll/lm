package io.terminus.galaxy.user.impl.service;

import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import io.terminus.common.model.PageInfo;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.galaxy.user.impl.dao.MainSellerRoleDao;
import io.terminus.galaxy.user.model.MainSellerRole;
import io.terminus.galaxy.user.service.MainSellerRoleReadService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @author Effet
 */
@Slf4j
@Service
public class MainSellerRoleReadServiceImpl implements MainSellerRoleReadService {

    private final MainSellerRoleDao mainSellerRoleDao;

    @Autowired
    public MainSellerRoleReadServiceImpl(MainSellerRoleDao mainSellerRoleDao) {
        this.mainSellerRoleDao = mainSellerRoleDao;
    }

    @Override
    public Response<MainSellerRole> findById(Long id) {
        try {
            return Response.ok(mainSellerRoleDao.findById(id));
        } catch (Exception e) {
            log.error("find operator role by id={} failed, cause:{}",
                    id, Throwables.getStackTraceAsString(e));
            return Response.fail("operator.role.find.fail");
        }
    }

    @Override
    public Response<List<MainSellerRole>> findByIds(List<Long> ids) {
        try {
            return Response.ok(mainSellerRoleDao.findByIds(ids));
        } catch (Exception e) {
            log.error("find operator role by ids={} failed, cause:{}",
                    ids, Throwables.getStackTraceAsString(e));
            return Response.fail("operator.role.find.fail");
        }
    }

    @Override
    public Response<List<MainSellerRole>> findByStatus(Integer status) {
        try {
            return Response.ok(mainSellerRoleDao.findByStatus(status));
        } catch (Exception e) {
            log.error("find operator roles by status={} failed, cause:{}",
                    status, Throwables.getStackTraceAsString(e));
            return Response.fail("operator.role.find.fail");
        }
    }

    @Override
    public Response<Paging<MainSellerRole>> pagination(String name, Integer status, Integer pageNo, Integer size) {
        try {
            PageInfo page = new PageInfo(pageNo, size);
            MainSellerRole criteria = new MainSellerRole();
            criteria.setStatus(status);
            criteria.setName(Strings.emptyToNull(name));
            return Response.ok(mainSellerRoleDao.paging(page.getOffset(), page.getLimit(), criteria));
        } catch (Exception e) {
            log.error("paging operator roles failed, status={}, pageNo={}, size={}, cause:{}",
                    status, pageNo, size, Throwables.getStackTraceAsString(e));
            return Response.fail("operator.role.paging.fail");
        }
    }
}
