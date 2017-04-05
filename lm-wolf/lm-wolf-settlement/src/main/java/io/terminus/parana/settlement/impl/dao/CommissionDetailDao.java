package io.terminus.parana.settlement.impl.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.settlement.model.CommissionDetail;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class CommissionDetailDao extends MyBatisDao {
   public List findBySystemNo(String systemNo) {
      return this.getSqlSession().selectList(this.sqlId("loadBySystemNo"), systemNo);
   }

   public List findByOrderId(Long orderId) {
      return this.getSqlSession().selectList(this.sqlId("loadByOrderId"), orderId);
   }

   public CommissionDetail findByOrderItemId(Long orderItemId) {
      return (CommissionDetail)this.getSqlSession().selectOne(this.sqlId("findByOrderItemId"), orderItemId);
   }

   public Boolean deleteByOrderId(Long orderId) {
      return Boolean.valueOf(this.getSqlSession().delete(this.sqlId("deleteByOrderId"), orderId) > 0);
   }
}
