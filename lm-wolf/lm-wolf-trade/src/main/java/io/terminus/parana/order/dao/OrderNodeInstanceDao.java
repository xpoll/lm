package io.terminus.parana.order.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.order.model.OrderNodeInstance;
import org.springframework.stereotype.Repository;

@Repository
public class OrderNodeInstanceDao extends MyBatisDao {
   public OrderNodeInstance getEntranceByFlowId(Long flowId) {
      return (OrderNodeInstance)this.getSqlSession().selectOne(this.sqlId("getEntranceByFlowId"), flowId);
   }
}
