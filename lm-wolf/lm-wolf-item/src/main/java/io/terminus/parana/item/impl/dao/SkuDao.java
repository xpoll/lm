package io.terminus.parana.item.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.item.model.Sku;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class SkuDao extends MyBatisDao {
   public List findByItemId(Long itemId) {
      return this.getSqlSession().selectList(this.sqlId("findByItemId"), itemId);
   }

   public List findByShopIdAndSkuCode(Long shopId, String skuCode) {
      return this.getSqlSession().selectList(this.sqlId("findByShopIdAndSkuCode"), ImmutableMap.of("shopId", shopId, "skuCode", skuCode));
   }

   public boolean updateByItemIdAndSkuCode(Long itemId, String skuCode, Sku sku) {
      sku.setItemId(itemId);
      sku.setSkuCode(skuCode);
      return this.getSqlSession().update(this.sqlId("updateByItemIdAndSkuCode"), sku) == 1;
   }

   public void updateStatusByItemIds(List itemIds, Integer status) {
      this.getSqlSession().update(this.sqlId("updateStatusByItemIds"), ImmutableMap.of("itemIds", itemIds, "status", status));
   }

   public void updateStatusByItemId(Long itemId, Integer status) {
      this.getSqlSession().update(this.sqlId("updateStatusByItemId"), ImmutableMap.of("itemId", itemId, "status", status));
   }

   public void updateStockQuantity(Long skuId, Integer delta) {
      this.getSqlSession().update(this.sqlId("updateStockQuantity"), ImmutableMap.of("skuId", skuId, "quantity", delta));
   }

   public void updateStatusBySkuId(Long skuId, Integer status) {
      this.getSqlSession().update(this.sqlId("updateStatusBySkuId"), ImmutableMap.of("skuId", skuId, "status", status));
   }
}
