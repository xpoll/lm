package io.terminus.parana.order.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.order.model.OrderTransferRule;
import org.springframework.stereotype.Repository;

@Repository
public class OrderTransferRuleDao extends MyBatisDao {
   public OrderTransferRule findByStartAndEnd(Long startId, Long endId) {
      return (OrderTransferRule)this.getSqlSession().selectOne(this.sqlId("findByStartAndEnd"), ImmutableMap.of("startId", startId, "endId", endId));
   }
}
