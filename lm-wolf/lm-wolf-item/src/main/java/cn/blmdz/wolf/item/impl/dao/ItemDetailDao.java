package cn.blmdz.wolf.item.impl.dao;

import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.item.model.ItemDetail;

@Repository
public class ItemDetailDao extends MyBatisDao {
   public ItemDetail findByItemId(Long itemId) {
      return (ItemDetail)this.getSqlSession().selectOne(this.sqlId("findByItemId"), itemId);
   }

   public Integer deleteByItemId(Long itemId) {
      return Integer.valueOf(this.getSqlSession().delete(this.sqlId("delete"), itemId));
   }
}
