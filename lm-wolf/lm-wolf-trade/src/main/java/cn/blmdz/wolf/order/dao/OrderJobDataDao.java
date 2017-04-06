package cn.blmdz.wolf.order.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;

@Repository
public class OrderJobDataDao extends MyBatisDao {
   public Long lastId() {
      return (Long)this.getSqlSession().selectOne(this.sqlId("lastId"));
   }

   public List listTo(Long lastId, Integer limit) {
      return this.getSqlSession().selectList(this.sqlId("listTo"), ImmutableMap.of("lastId", lastId, "limit", limit));
   }

   public void updateBy(Long orderId, Integer orderType, Long actionInstanceId, Integer status) {
      this.getSqlSession().update(this.sqlId("updateBy"), ImmutableMap.of("orderId", orderId, "orderType", orderType, "actionInstanceId", actionInstanceId, "status", status));
   }
}
