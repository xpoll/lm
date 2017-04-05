package io.terminus.parana.shop.impl.dao;

import com.google.common.base.MoreObjects;
import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.common.utils.JsonMapper;
import io.terminus.parana.shop.model.Shop;
import java.util.List;
import java.util.Map;
import org.springframework.stereotype.Repository;

@Repository
public class ShopDao extends MyBatisDao {
   public Shop findByUserId(Long userId) {
      return (Shop)this.getSqlSession().selectOne(this.sqlId("findByUserId"), userId);
   }

   public Shop findByName(String shopName) {
      return (Shop)this.getSqlSession().selectOne(this.sqlId("findByName"), shopName);
   }

   public Shop findByOuterId(String outerId) {
      return (Shop)this.getSqlSession().selectOne(this.sqlId("findByOuterId"), outerId);
   }

   public void updateStatus(Long shopId, Integer status) {
      this.getSqlSession().update(this.sqlId("updateStatus"), ImmutableMap.of("id", shopId, "status", status));
   }

   public void updateTags(Long shopId, Map tags) {
      this.getSqlSession().update(this.sqlId("updateTags"), ImmutableMap.of("id", shopId, "tagsJson", JsonMapper.JSON_NON_DEFAULT_MAPPER.toJson(tags)));
   }

   public Long maxId() {
      Long id = (Long)this.getSqlSession().selectOne(this.sqlId("lastId"));
      return (Long)MoreObjects.firstNonNull(id, Long.valueOf(0L));
   }

   public List listTo(Long lastId, int limit) {
      return this.getSqlSession().selectList(this.sqlId("listTo"), ImmutableMap.of("lastId", lastId, "limit", Integer.valueOf(limit)));
   }

   public List listSince(Long lastId, String since, int limit) {
      return this.getSqlSession().selectList(this.sqlId("listSince"), ImmutableMap.of("lastId", lastId, "limit", Integer.valueOf(limit), "since", since));
   }
}
