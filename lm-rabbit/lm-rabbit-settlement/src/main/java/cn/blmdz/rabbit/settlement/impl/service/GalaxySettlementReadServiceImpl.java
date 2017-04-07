package cn.blmdz.rabbit.settlement.impl.service;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkState;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Throwables;
import com.google.common.collect.Lists;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.rabbit.settlement.dto.CommissionDto;
import cn.blmdz.rabbit.settlement.enums.CommissionBusinessType;
import cn.blmdz.rabbit.settlement.enums.SettlementRedisBusinessType;
import cn.blmdz.rabbit.settlement.impl.dao.SettlementSumOfDailyDao;
import cn.blmdz.rabbit.settlement.impl.dao.SettlementSumOfShopDailyDao;
import cn.blmdz.rabbit.settlement.impl.dao.redis.SettlementRedisDao;
import cn.blmdz.rabbit.settlement.model.SettlementSumOfDaily;
import cn.blmdz.rabbit.settlement.model.SettlementSumOfShopDaily;
import cn.blmdz.rabbit.settlement.service.GalaxySettlementReadService;
import cn.blmdz.wolf.settlement.enums.CommissionDetailType;
import cn.blmdz.wolf.settlement.enums.CommissionType;
import cn.blmdz.wolf.settlement.model.CommissionDetail;
import cn.blmdz.wolf.settlement.model.CommissionRule;
import cn.blmdz.wolf.settlement.service.SettlementReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/22/16
 * Time: 9:27 PM
 */
@Component @Slf4j
public class GalaxySettlementReadServiceImpl implements GalaxySettlementReadService {

    @Autowired
    private SettlementReadService settlementReadService;
    @Autowired
    private SettlementRedisDao settlementRedisDao;
    @Autowired
    private SettlementSumOfShopDailyDao settlementSumOfShopDailyDao;
    @Autowired
    private SettlementSumOfDailyDao settlementSumOfDailyDao;

    @Override
    public Response<CommissionDto> countCommission(Long fee, CommissionType type, CommissionBusinessType businessType,CommissionDetailType commissionDetailType, Long businessId) {
        Response<CommissionDto> result = new Response<CommissionDto>();
        try {
            checkArgument(Arguments.notNull(fee),"commission.fee.invalid");
            checkArgument(Arguments.notNull(type),"commission.type.invalid");
            checkArgument(Arguments.notNull(businessType),"commission.business.type.invalid");
            checkArgument(Arguments.notNull(businessId),"commission.business.id.invalid");
            CommissionDto dto = new CommissionDto();

            Response<CommissionRule> ruleRes = settlementReadService.findMatchCommissionRule(businessId,businessType.value(),type.value());
            checkState(ruleRes.isSuccess(),ruleRes.getError());
            List<CommissionDetail> commissionDetails = Lists.newArrayList();
            //如果规则不存在 则返回0L
            if(Arguments.isNull(ruleRes.getResult())){
                dto.setCommisson(0L);
                dto.setCommissionDetails(commissionDetails);
                result.setResult(dto);
                return result;
            }
            CommissionRule rule = ruleRes.getResult();
            Long commission = new BigDecimal(fee).multiply(new BigDecimal(rule.getRate()).divide(new BigDecimal(10000))).setScale(0, BigDecimal.ROUND_UP).longValue();//取0位小数 向上取整
            CommissionDetail detail = new CommissionDetail();
            detail.setCommissionRuleId(rule.getId());
            detail.setCommission(commission);
            detail.setRate(rule.getRate());
            detail.setPrice(fee);
            detail.setLowestFee(0L);
            detail.setType(commissionDetailType.value());
            detail.setDescription("");
            commissionDetails.add(detail);
            dto.setCommisson(commission);
            dto.setCommissionDetails(commissionDetails);

            result.setResult(dto);

        }catch (IllegalArgumentException | IllegalStateException e){
            log.error("count commission fail where fee: {} type: {} business type: {} business id: {},error: {}",fee,type.toString(),businessType.toString(),businessId, e.getMessage());
            result.setError(e.getMessage());
        }catch (Exception e){
            log.error("count commission fail where fee: {} type: {} business type: {} business id: {},cause: {}",fee,type.toString(),businessType.toString(),businessId, Throwables.getStackTraceAsString(e));
            result.setError("count.commission.fail");
        }
        return result;
    }

    @Override
    public Response<List<Long>> getSettlementReidsIds(SettlementRedisBusinessType type) {
        Response<List<Long>> result = new Response<List<Long>>();
        List<Long> ids = Lists.newArrayList();
        try {
            checkArgument(Arguments.notNull(type),"settlement.redis.business.type.is.null");

            Set<String> setIds = settlementRedisDao.getRedisIds(type.value());
            if(Arguments.isNull(setIds)||setIds.size()==0){
                result.setResult(ids);
                return result;
            }
            for(String id : setIds){
                ids.add(Long.valueOf(id));
            }
            result.setResult(ids);

        }catch (IllegalArgumentException e){
            log.error("get settlement redis ids  fail,error: {} ",e.getMessage());
            result.setError(e.getMessage());
        }catch (Exception e){
            log.error("get settlement redis ids where business type : {} fail,cause: {} ",type.toString(),Throwables.getStackTraceAsString(e));
            result.setError("get.settlement.redis.ids.fail");
        }
        return result;
    }

    @Override
    public Response<SettlementSumOfShopDaily> findSettlementSumOfShopDailyByShopIdAndSumAt(Long shopId, Date sumAt) {
        Response<SettlementSumOfShopDaily> result = new Response<SettlementSumOfShopDaily>();
        try {
            result.setResult(settlementSumOfShopDailyDao.findBySumAt(shopId,sumAt));
        }catch (Exception e){
            log.error("find settlement sum of shop daily by shop id: {} sum at: {} fail,cause: {}",shopId,sumAt,Throwables.getStackTraceAsString(e));
            result.setError("query.settlement.sum.of.shop.fail");
        }
        return result;
    }

    @Override
    public Response<SettlementSumOfDaily> findSettlementSumOfDailyBySumAt(Date sumAt) {
        Response<SettlementSumOfDaily> result = new Response<SettlementSumOfDaily>();
        try {
            result.setResult(settlementSumOfDailyDao.findBySumAt(sumAt));
        }catch (Exception e){
            log.error("find settlement sum of  daily by sum at: {} fail,cause: {}",sumAt,Throwables.getStackTraceAsString(e));
            result.setError("query.settlement.sum..fail");
        }
        return result;    }
}
