package io.terminus.parana.order.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import java.util.List;
import org.springframework.stereotype.Repository;

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
