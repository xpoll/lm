package io.terminus.parana.order.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class SkuOrderDao extends MyBatisDao {
   public List findByParentId(Long parentId) {
      return this.getSqlSession().selectList(this.sqlId("findByParentId"), parentId);
   }

   public List findByParentIds(List parentIds) {
      return this.getSqlSession().selectList(this.sqlId("findByParentIds"), parentIds);
   }
}
