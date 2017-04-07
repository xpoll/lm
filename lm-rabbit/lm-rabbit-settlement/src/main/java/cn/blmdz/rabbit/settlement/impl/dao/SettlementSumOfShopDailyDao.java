package cn.blmdz.rabbit.settlement.impl.dao;

import java.util.Date;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.rabbit.settlement.model.SettlementSumOfShopDaily;

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
