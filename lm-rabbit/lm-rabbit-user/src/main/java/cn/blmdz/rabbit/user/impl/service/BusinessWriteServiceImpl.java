package cn.blmdz.rabbit.user.impl.service;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.user.impl.dao.BusinessDao;
import cn.blmdz.rabbit.user.model.Business;
import cn.blmdz.rabbit.user.service.BusinessWriteService;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by liushaofei on 16/8/9.
 */
@Slf4j
@Service
public class BusinessWriteServiceImpl implements BusinessWriteService {
    private final BusinessDao businessDao;

    @Autowired
    public BusinessWriteServiceImpl(BusinessDao businessDao){
        this.businessDao = businessDao;
    }

    @Override
    public Response<Boolean> insert(Business business) {
        try {
            if (businessDao.insert(business) != 1){
                log.error("business.create.fail");
                return Response.fail("business create fail");
            }
            return Response.ok();
        } catch (Exception e){
            log.error("failed to create {}, cause:{}", business, Throwables.getStackTraceAsString(e));
            return Response.fail("business.create.fail");
        }

    }

    @Override
    public Response<Boolean> delete(Business business) {
        try{
            if (businessDao.delete(business) != 1){
                log.error("business.delete.fail");
                return Response.fail("business.delete.fail");
            }
            return Response.ok();
        } catch (Exception e){
            log.error("failed to delete {}, cause:{}", business, Throwables.getStackTraceAsString(e));
            return Response.fail("business.delete.fail");
        }
    }

    @Override
    public Response<Boolean> update(Map<String, Object> map) {

        try{
            if (businessDao.updateOne(map) != 1){
                log.error("business.update.fail");
                return Response.fail("business.delete.fail");
            }
            return Response.ok();
        } catch (Exception e){
            log.error("failed to create {}, cause:{}", map, Throwables.getStackTraceAsString(e));
            return Response.fail("business.update.fail");
        }
    }
}
