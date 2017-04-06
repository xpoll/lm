package cn.blmdz.wolf.order.dao;

import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.order.model.OrderNodeInstance;

@Repository
public class OrderNodeInstanceDao extends MyBatisDao {
   public OrderNodeInstance getEntranceByFlowId(Long flowId) {
      return (OrderNodeInstance)this.getSqlSession().selectOne(this.sqlId("getEntranceByFlowId"), flowId);
   }
}
