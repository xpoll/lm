package io.terminus.galaxy.settlement.service;

import io.terminus.common.model.Response;
import io.terminus.galaxy.settlement.enums.SettlementRedisBusinessType;
import io.terminus.galaxy.settlement.model.SettlementSumOfDaily;
import io.terminus.galaxy.settlement.model.SettlementSumOfShopDaily;

import java.util.Date;
import java.util.List;

/**
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/22/16
 * Time: 9:26 PM
 */
public interface GalaxySettlementWriteService {


    /**
     * 保存对应业务数据主键ids到redis
     * @param type 业务类型
     * @param ids 主键ids
     * @return 是否创建成功
     */
    Response<Boolean> saveSettlementRedisIds(SettlementRedisBusinessType type,List<Long> ids);

    /**
     * 删除指定业务类型redis中ids
     * @param type {@link io.terminus.galaxy.settlement.enums.SettlementRedisBusinessType}
     * @return 是否删除成功
     */
    Response<Boolean> delSettlementRedisIds(SettlementRedisBusinessType type);

    /**
     * 删除指定店铺主键 日期的店铺日汇总
     * @param shopId 店铺主键
     * @param sumAt 汇总日期
     * @return 是否删除成功
     */
    Response<Boolean> delSettlementSumOfShopDaily(Long shopId,Date sumAt);

    /**
     * 删除指定日期的平台日汇总
     * @param sumAt 汇总日期
     * @return 是否删除成功
     */
    Response<Boolean> delSettlementSumOfPlatformDaily(Date sumAt);


    /**
     * 创建店铺日汇总
     * @param daily 店铺日汇总
     * @return 是否创建成功
     */
    Response<Boolean> createSettlementSumOfShopDaily(SettlementSumOfShopDaily daily);

    /**
     * 创建平台日汇总
     * @param daily 平台日汇总
     * @return 是否创建成功
     */
    Response<Boolean> createSettlementSumOfDaily(SettlementSumOfDaily daily);
}
