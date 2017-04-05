package io.terminus.parana.settlement.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.settlement.model.CommissionRule;
import org.springframework.stereotype.Repository;

@Repository
public class CommissionRuleDao extends MyBatisDao {
   public CommissionRule findByBusinessIdAndBusinessTypeAndCommissionType(Long businessId, Integer businessType, Integer commissionType) {
      return (CommissionRule)this.getSqlSession().selectOne(this.sqlId("findByBusinessIdAndBusinessTypeAndCommissionType"), ImmutableMap.of("businessId", businessId, "businessType", businessType, "commissionType", commissionType));
   }
}
