package io.terminus.galaxy.settlement.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.galaxy.settlement.model.SettlementSumOfShopDaily;
import org.springframework.stereotype.Repository;

import java.util.Date;

/**
 * 店铺结算每日汇总Dao
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 14/12/22
 * Time: 下午4:49
 */
@Repository
public class SettlementSumOfShopDailyDao extends MyBatisDao<SettlementSumOfShopDaily> {

    public SettlementSumOfShopDaily findBySumAt(Long shopId,Date sumAt){
        return getSqlSession().selectOne(sqlId("findBySumAt"), ImmutableMap.of("sumAt", sumAt, "shopId", shopId));
    }

    public Boolean deleteSumAt(Long shopId,Date sumAt){
        return getSqlSession().delete(sqlId("deleteSumAt"), ImmutableMap.of("sumAt", sumAt, "shopId", shopId))>0;
    }

}
