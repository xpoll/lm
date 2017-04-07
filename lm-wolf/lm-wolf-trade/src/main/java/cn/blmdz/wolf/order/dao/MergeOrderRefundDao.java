package cn.blmdz.wolf.order.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.order.model.MergeOrderRefund;

@Repository
public class MergeOrderRefundDao extends MyBatisDao<MergeOrderRefund> {
   public List findByParentId(Long parentId) {
      return this.getSqlSession().selectList(this.sqlId("findByParentId"), parentId);
   }

   public List findByParentIds(List parentIds) {
      return this.getSqlSession().selectList(this.sqlId("findByParentIds"), parentIds);
   }
}
