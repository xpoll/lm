package io.terminus.parana.item.impl.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.item.model.ItemAttribute;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class ItemAttributeDao extends MyBatisDao {
   public ItemAttribute findByItemId(Long itemId) {
      return (ItemAttribute)this.getSqlSession().selectOne(this.sqlId("findByItemId"), itemId);
   }

   public List findByItemIds(List itemIds) {
      return this.getSqlSession().selectList(this.sqlId("findByItemIds"), itemIds);
   }

   public boolean updateByItemId(ItemAttribute itemAttribute) {
      return this.getSqlSession().update(this.sqlId("update"), itemAttribute) == 1;
   }

   public boolean deleteByItemId(Long itemId) {
      return this.getSqlSession().delete(this.sqlId("delete"), itemId) == 1;
   }
}
