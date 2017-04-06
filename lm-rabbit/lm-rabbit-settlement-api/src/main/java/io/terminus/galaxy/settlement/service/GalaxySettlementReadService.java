package io.terminus.galaxy.settlement.service;

import io.terminus.common.model.Response;
import io.terminus.galaxy.settlement.dto.CommissionDto;
import io.terminus.galaxy.settlement.enums.CommissionBusinessType;
import io.terminus.galaxy.settlement.enums.SettlementRedisBusinessType;
import io.terminus.galaxy.settlement.model.SettlementSumOfDaily;
import io.terminus.galaxy.settlement.model.SettlementSumOfShopDaily;
import io.terminus.parana.settlement.enums.CommissionDetailType;
import io.terminus.parana.settlement.enums.CommissionType;

import java.util.Date;
import java.util.List;

/**
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/22/16
 * Time: 9:25 PM
 */
public interface GalaxySettlementReadService {


    /**
     * 计算佣金
     * @param fee 参与计算的金额
     * @param type 抽佣类型
     * @param businessType 抽佣业务类型
     * @param businessId 抽佣业务id
     * @return 佣金
     */
    Response<CommissionDto> countCommission(Long fee,CommissionType type,CommissionBusinessType businessType,CommissionDetailType commissionDetailType,Long businessId);


    /**
     * 获取具体业务类型redis中的主键集合
     * @param type 业务类型 {@link io.terminus.galaxy.settlement.enums.SettlementRedisBusinessType}
     * @return 主键id集合
     */
    Response<List<Long>> getSettlementReidsIds(SettlementRedisBusinessType type);


    /**
     * 根据店铺主键 和 汇总日期查询店铺日汇总
     * @param shopId 店铺主键
     * @param sumAt 汇总日期
     * @return 店铺日汇总
     */
    Response<SettlementSumOfShopDaily> findSettlementSumOfShopDailyByShopIdAndSumAt(Long shopId,Date sumAt);



    /**
     * 根据汇总日期查询平台日汇总
     * @param sumAt 汇总日期
     * @return 平台日汇总
     */
    Response<SettlementSumOfDaily> findSettlementSumOfDailyBySumAt(Date sumAt);
}
