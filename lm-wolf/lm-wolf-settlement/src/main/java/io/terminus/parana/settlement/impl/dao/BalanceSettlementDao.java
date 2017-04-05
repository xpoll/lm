package io.terminus.parana.settlement.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.settlement.enums.BalanceType;
import io.terminus.parana.settlement.model.BalanceSettlement;
import org.springframework.stereotype.Repository;

@Repository
public class BalanceSettlementDao extends MyBatisDao {
   public BalanceSettlement findByOrderIdAndType(Long orderId, BalanceType type) {
      return (BalanceSettlement)this.getSqlSession().selectOne(this.sqlId("findByOrderIdAndType"), ImmutableMap.of("orderId", orderId, "type", Integer.valueOf(type.value())));
   }
}
