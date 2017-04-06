package cn.blmdz.wolf.settlement.impl.dao;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.settlement.model.CommissionRule;

@Repository
public class CommissionRuleDao extends MyBatisDao {
   public CommissionRule findByBusinessIdAndBusinessTypeAndCommissionType(Long businessId, Integer businessType, Integer commissionType) {
      return (CommissionRule)this.getSqlSession().selectOne(this.sqlId("findByBusinessIdAndBusinessTypeAndCommissionType"), ImmutableMap.of("businessId", businessId, "businessType", businessType, "commissionType", commissionType));
   }
}
