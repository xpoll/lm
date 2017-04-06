package io.terminus.galaxy.user.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.PageInfo;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Params;
import io.terminus.galaxy.user.impl.dao.BusinessDao;
import io.terminus.galaxy.user.model.Business;
import io.terminus.galaxy.user.service.BusinessReadService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by liushaofei on 16/7/29.
 */
@Slf4j
@Service
public class BusinessReadServiceImpl implements BusinessReadService {
    private final BusinessDao businessDao;

    @Autowired
    public BusinessReadServiceImpl(BusinessDao businessDao){
        this.businessDao = businessDao;
    }

    @Override
    public Response<Paging<Business>> select(Business business,Integer pageIndex, Integer pageSize) {
        return businessDao.select(business, pageIndex, pageSize);
    }

    @Override
    public Response<Paging<Business>> selectByBusinessNameAndType(String businessName, Integer type, Integer pageNo, Integer pageSize) {
        try {
            PageInfo page = new PageInfo(pageNo, pageSize);
            Map<String, Object> params = new HashMap<>();
            Business business = new Business();
            business.setBusinessName(Params.trimToNull(businessName));
            business.setType(type);
            params.put("business", business);
            return Response.ok(businessDao.paging(page.getOffset(), page.getLimit(), params));
        } catch (Exception e) {
            log.error("select by businessName={} and type={}, failed, pageNo={}, pageSize={}, cause:{}",
                    businessName, type, pageNo, pageSize, Throwables.getStackTraceAsString(e));
            return Response.fail("business.paging.fail");
        }
    }

    @Override
    public List<Business> findByIds(List<Long> ids) {
        return businessDao.findByIds(ids);
    }

    @Override
    public Business findById(Long id) {
        List<Long> ids = new ArrayList<>();
        ids.add(id);
        List<Business> list = findByIds(ids);
        if (list != null && list.size() == 1){
            return list.get(0);
        }
        return null;
    }




}
