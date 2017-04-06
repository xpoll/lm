package cn.blmdz.wolf.settlement.impl.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.settlement.model.CommissionDetail;

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
