package cn.blmdz.wolf.settlement.impl.dao;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.settlement.enums.BalanceType;
import cn.blmdz.wolf.settlement.model.BalanceSettlement;

@Repository
public class BalanceSettlementDao extends MyBatisDao {
   public BalanceSettlement findByOrderIdAndType(Long orderId, BalanceType type) {
      return (BalanceSettlement)this.getSqlSession().selectOne(this.sqlId("findByOrderIdAndType"), ImmutableMap.of("orderId", orderId, "type", Integer.valueOf(type.value())));
   }
}
