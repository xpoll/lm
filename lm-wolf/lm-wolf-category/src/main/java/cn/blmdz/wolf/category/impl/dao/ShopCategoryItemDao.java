package cn.blmdz.wolf.category.impl.dao;

import java.util.Collections;
import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.base.MoreObjects;
import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.mysql.dao.MyBatisDao;

@Repository
public class ShopCategoryItemDao extends MyBatisDao {
   public Paging findByShopIdAndCategoryId(Long shopId, Long categoryId, Integer offset, Integer size) {
      Long total = (Long)this.getSqlSession().selectOne(this.sqlId("countByShopIdAndCategoryId"), ImmutableMap.of("shopId", shopId, "shopCategoryId", categoryId));
      total = (Long)MoreObjects.firstNonNull(total, Long.valueOf(0L));
      if(total.longValue() == 0L) {
         return new Paging(Long.valueOf(0L), Collections.emptyList());
      } else {
         List<Long> result = this.getSqlSession().selectList(this.sqlId("findByShopIdAndCategoryId"), ImmutableMap.of("shopId", shopId, "shopCategoryId", categoryId, "offset", offset, "limit", size));
         return new Paging(total, result);
      }
   }

   public Paging findUnknownByShopId(Long shopId, Integer offset, Integer limit) {
      Long total = (Long)this.getSqlSession().selectOne(this.sqlId("countUnknownByShopId"), ImmutableMap.of("shopId", shopId));
      total = (Long)MoreObjects.firstNonNull(total, Long.valueOf(0L));
      if(total.longValue() == 0L) {
         return new Paging(Long.valueOf(0L), Collections.emptyList());
      } else {
         List<Long> result = this.getSqlSession().selectList(this.sqlId("findUnknownByShopId"), ImmutableMap.of("shopId", shopId, "offset", offset, "limit", limit));
         return new Paging(total, result);
      }
   }

   public List findByShopIdAndItemId(Long shopId, Long itemId) {
      return this.getSqlSession().selectList(this.sqlId("findByShopIdAndItemId"), ImmutableMap.of("shopId", shopId, "itemId", itemId));
   }

   public List findByShopIdAndItemIds(Long shopId, List itemIds) {
      return this.getSqlSession().selectList(this.sqlId("findByShopIdAndItemIds"), ImmutableMap.of("shopId", shopId, "itemIds", itemIds));
   }

   public void deleteByShopIdAndCategoryId(Long shopId, Long categoryId) {
      this.getSqlSession().delete(this.sqlId("deleteBy"), ImmutableMap.of("shopId", shopId, "shopCategoryId", categoryId));
   }

   public void deleteByShopIdAndItemId(Long shopId, Long itemId) {
      this.getSqlSession().delete(this.sqlId("deleteBy"), ImmutableMap.of("shopId", shopId, "itemId", itemId));
   }
}
