package io.terminus.galaxy.user.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.PageInfo;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.galaxy.user.impl.dao.OperatorDao;
import io.terminus.galaxy.user.model.Operator;
import io.terminus.galaxy.user.service.OperatorReadService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author Effet
 */
@Slf4j
@Service
public class OperatorReadServiceImpl implements OperatorReadService {

    private final OperatorDao operatorDao;

    @Autowired
    public OperatorReadServiceImpl(OperatorDao operatorDao) {
        this.operatorDao = operatorDao;
    }

    @Override
    public Response<Operator> findByUserId(Long userId) {
        try {
            Operator operator = operatorDao.findByUserId(userId);
            return Response.ok(operator);
        } catch (Exception e) {
            log.error("find operator by userId={} failed, cause:{}",
                    userId, Throwables.getStackTraceAsString(e));
            return Response.fail("operator.find.fail");
        }
    }

    @Override
    public Response<Paging<Operator>> pagination(Long roleId, Integer status, Integer pageNo, Integer size) {
        try {
            PageInfo page = new PageInfo(pageNo, size);
            Operator criteria = new Operator();
            criteria.setStatus(status);
            criteria.setRoleId(roleId);
            return Response.ok(operatorDao.paging(page.getOffset(), page.getLimit(), criteria));
        } catch (Exception e) {
            log.error("paging operator by roleId={}, status={}, pageNo={} size={} failed, cause:{}",
                    roleId, status, pageNo, size, Throwables.getStackTraceAsString(e));
            return Response.fail("operator.find.fail");
        }
    }
}
