package cn.blmdz.wolf.order.dao;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.order.model.OrderTransferRule;

@Repository
public class OrderTransferRuleDao extends MyBatisDao {
   public OrderTransferRule findByStartAndEnd(Long startId, Long endId) {
      return (OrderTransferRule)this.getSqlSession().selectOne(this.sqlId("findByStartAndEnd"), ImmutableMap.of("startId", startId, "endId", endId));
   }
}
