package io.terminus.parana.item.impl.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.item.model.ItemDetail;
import org.springframework.stereotype.Repository;

@Repository
public class ItemDetailDao extends MyBatisDao {
   public ItemDetail findByItemId(Long itemId) {
      return (ItemDetail)this.getSqlSession().selectOne(this.sqlId("findByItemId"), itemId);
   }

   public Integer deleteByItemId(Long itemId) {
      return Integer.valueOf(this.getSqlSession().delete(this.sqlId("delete"), itemId));
   }
}
