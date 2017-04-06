package io.terminus.galaxy.settlement.impl.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.galaxy.settlement.model.SettlementSumOfDaily;
import org.springframework.stereotype.Repository;

import java.util.Date;

/**
 * 结算每日汇总Dao
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 14/12/22
 * Time: 下午4:49
 */
@Repository
public class SettlementSumOfDailyDao extends MyBatisDao<SettlementSumOfDaily> {

    public SettlementSumOfDaily findBySumAt(Date sumAt){
        return getSqlSession().selectOne(sqlId("findBySumAt"), sumAt);
    }

    public Boolean deleteSumAt(Date sumAt){
        return getSqlSession().delete(sqlId("deleteSumAt"),sumAt)>0;
    }

    /**
     * 通过Id 修改对应的commission
     * @param settlementSumOfDaily
     * @return
     */
    public Boolean updateCommissionById(SettlementSumOfDaily settlementSumOfDaily){
        return getSqlSession().update(sqlId("updateCommissionById"),settlementSumOfDaily) == 1;
    }
}
