package io.terminus.parana.item.impl.dao;

import com.google.common.base.MoreObjects;
import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.common.utils.JsonMapper;
import java.util.List;
import java.util.Map;
import org.springframework.stereotype.Repository;

@Repository
public class ItemDao extends MyBatisDao {
   public List findByShopIdAndCode(Long shopId, String itemCode) {
      return this.getSqlSession().selectList(this.sqlId("findByCode"), ImmutableMap.of("shopId", shopId, "itemCode", itemCode));
   }

   public List findByItemCode(String itemCode) {
      return this.getSqlSession().selectList(this.sqlId("findByCode"), ImmutableMap.of("itemCode", itemCode));
   }

   public void updateStatus(Long itemId, Integer status) {
      this.getSqlSession().update(this.sqlId("updateStatus"), ImmutableMap.of("id", itemId, "status", status));
   }

   public boolean updateStatusByShopIdAndItemId(Long shopId, Long itemId, Integer status) {
      return this.getSqlSession().update(this.sqlId("updateStatusByShopIdAndItemId"), ImmutableMap.of("id", itemId, "status", status, "shopId", shopId)) == 1;
   }

   public void updateItemInfoMd5(Long itemId, String itemInfoMd5) {
      this.getSqlSession().update(this.sqlId("updateItemInfoMd5"), ImmutableMap.of("id", itemId, "itemInfoMd5", itemInfoMd5));
   }

   public void batchUpdateStatus(List ids, Integer status) {
      this.getSqlSession().update(this.sqlId("batchUpdateStatus"), ImmutableMap.of("ids", ids, "status", status));
   }

   public boolean batchUpdateStatusByShopIdAndIds(Long shopId, List ids, Integer status) {
      return this.getSqlSession().update(this.sqlId("batchUpdateStatusByShopIdAndIds"), ImmutableMap.of("shopId", shopId, "ids", ids, "status", status)) == ids.size();
   }

   public void batchUpdateStatusByShopId(Long shopId, Integer status) {
      this.getSqlSession().update(this.sqlId("batchUpdateStatusByShopId"), ImmutableMap.of("shopId", shopId, "status", status));
   }

   public void updateTags(Long itemId, Map tags, String itemInfoMd5) {
      this.getSqlSession().update(this.sqlId("updateTags"), ImmutableMap.of("itemId", itemId, "tagsJson", JsonMapper.JSON_NON_DEFAULT_MAPPER.toJson(tags), "itemInfoMd5", itemInfoMd5));
   }

   public Long maxId() {
      Long id = (Long)this.getSqlSession().selectOne(this.sqlId("maxId"));
      return (Long)MoreObjects.firstNonNull(id, Long.valueOf(0L));
   }

   public List listSince(Long lastId, String since, int limit) {
      return this.getSqlSession().selectList(this.sqlId("listSince"), ImmutableMap.of("lastId", lastId, "limit", Integer.valueOf(limit), "since", since));
   }

   public void updateSaleQuantity(Long itemId, Integer delta) {
      this.getSqlSession().update(this.sqlId("updateSaleQuantity"), ImmutableMap.of("itemId", itemId, "quantity", delta));
   }
}
