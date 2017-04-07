package cn.blmdz.rabbit.settlement.impl.service;

import static com.google.common.base.Preconditions.checkArgument;

import java.util.Date;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.rabbit.settlement.enums.SettlementRedisBusinessType;
import cn.blmdz.rabbit.settlement.impl.dao.SettlementSumOfDailyDao;
import cn.blmdz.rabbit.settlement.impl.dao.SettlementSumOfShopDailyDao;
import cn.blmdz.rabbit.settlement.impl.dao.redis.SettlementRedisDao;
import cn.blmdz.rabbit.settlement.model.SettlementSumOfDaily;
import cn.blmdz.rabbit.settlement.model.SettlementSumOfShopDaily;
import cn.blmdz.rabbit.settlement.service.GalaxySettlementWriteService;
import lombok.extern.slf4j.Slf4j;

/**
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/22/16
 * Time: 9:28 PM
 */
@Component @Slf4j
public class GalaxySettlementWriteServiceImpl implements GalaxySettlementWriteService {

    @Autowired
    private SettlementRedisDao settlementRedisDao;
    @Autowired
    private SettlementSumOfDailyDao settlementSumOfDailyDao;
    @Autowired
    private SettlementSumOfShopDailyDao settlementSumOfShopDailyDao;

    @Override
    public Response<Boolean> saveSettlementRedisIds(SettlementRedisBusinessType type, List<Long> ids) {
        Response<Boolean> result = new Response<Boolean>();
        try {
            checkArgument(Arguments.notNull(type),"settlement.redis.business.type.is.null");
            checkArgument(!Arguments.isNullOrEmpty(ids),"settlement.id.is.null");

            settlementRedisDao.generateRedisIds(type.value(),ids);

            result.setResult(Boolean.TRUE);

        }catch (IllegalArgumentException e){
            log.error("save settlement redis ids where business fail,error: {}",e.getMessage());
            result.setError(e.getMessage());
        }catch (Exception e){
            log.error("save settlement redis ids where business type :{} ids:{} fail,cause: {}",type.toString(),ids.toArray(), Throwables.getStackTraceAsString(e));
            result.setError("save.settlement.redis.id.fail");
        }
        return result;
    }

    @Override
    public Response<Boolean> delSettlementRedisIds(SettlementRedisBusinessType type) {
        Response<Boolean> result = new Response<Boolean>();
        try {
            checkArgument(Arguments.notNull(type),"settlement.redis.bussiness.type.is.null");
            settlementRedisDao.delRedisIds(type.value());

            result.setResult(Boolean.TRUE);
        }catch (IllegalArgumentException e){
            log.error("del settlement redis ids where business type fail ,error: {}",e.getMessage());
            result.setError(e.getMessage());
        }catch (Exception e){
            log.error("del settlement redis ids where business type: {} fail ,cause: {}",type.toString(),Throwables.getStackTraceAsString(e));
            result.setError("del.settlement.redis.ids.fail");
        }
        return result;
    }

    @Override
    public Response<Boolean> delSettlementSumOfShopDaily(Long shopId, Date sumAt) {
        Response<Boolean> result = new Response<Boolean>();
        try {
            settlementSumOfShopDailyDao.deleteSumAt(shopId,sumAt);
            result.setResult(Boolean.TRUE);
        }catch (Exception e){
            log.error("del settlement sum of shop daily by shop id: {} sum at:{} fail,cause: {}",shopId,sumAt,Throwables.getStackTraceAsString(e));
            result.setError("del.settlement.sum.of.shop.fail");
        }
        return result;
    }

    @Override
    public Response<Boolean> delSettlementSumOfPlatformDaily(Date sumAt) {
        Response<Boolean> result = new Response<Boolean>();
        try {
            settlementSumOfDailyDao.deleteSumAt(sumAt);
            result.setResult(Boolean.TRUE);

        }catch (Exception e){
            log.error("del settlement sum of platform daily by sum at:{} fail,cause: {}",sumAt,Throwables.getStackTraceAsString(e));
            result.setError("del.settlement.sum.of.platform.fail");
        }
        return result;    }

    @Override
    public Response<Boolean> createSettlementSumOfShopDaily(SettlementSumOfShopDaily daily) {
        Response<Boolean> result = new Response<Boolean>();
        try {

            settlementSumOfShopDailyDao.create(daily);

            result.setResult(Boolean.TRUE);

        }catch (Exception e){
            log.error("create settlement sum of shop daily: {} fail,cause: {}",daily,Throwables.getStackTraceAsString(e));
            result.setError("create.settlement.sum.of.shop.daily.fail");
        }
        return result;
    }

    @Override
    public Response<Boolean> createSettlementSumOfDaily(SettlementSumOfDaily daily) {
        Response<Boolean> result = new Response<Boolean>();
        try {

            settlementSumOfDailyDao.create(daily);

            result.setResult(Boolean.TRUE);

        }catch (Exception e){
            log.error("create settlement sum of platform daily: {} fail,cause: {}",daily,Throwables.getStackTraceAsString(e));
            result.setError("create.settlement.sum.of.platform.daily.fail");
        }
        return result;    }
}
