package io.terminus.parana.spu.impl.dao;

import com.google.common.base.MoreObjects;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.common.utils.JsonMapper;
import io.terminus.parana.spu.model.Spu;
import java.util.List;
import java.util.Map;
import org.springframework.stereotype.Repository;

@Repository
public class SpuDao extends MyBatisDao {
   public List findByCategoryId(Long categoryId) {
      return this.getSqlSession().selectList(this.sqlId("findByCategoryId"), categoryId);
   }

   public Spu findByCode(String spuCode) {
      return (Spu)this.getSqlSession().selectOne(this.sqlId("findByCode"), spuCode);
   }

   public List findByCategoryIdAndFuzzName(Long categoryId, String name) {
      return this.getSqlSession().selectList(this.sqlId("findByCategoryIdAndFuzzName"), ImmutableMap.of("categoryId", categoryId, "name", name));
   }

   public Spu findByCategoryIdAndName(Long categoryId, String name) {
      return (Spu)this.getSqlSession().selectOne(this.sqlId("findByCategoryIdAndName"), ImmutableMap.of("categoryId", categoryId, "name", name));
   }

   public void updateStatus(Long spuId, Integer status) {
      this.getSqlSession().update(this.sqlId("updateStatus"), ImmutableMap.of("id", spuId, "status", status));
   }

   public void updateSpuInfoMd5(Long spuId, String spuInfoMd5) {
      this.getSqlSession().update(this.sqlId("updateSpuInfoMd5"), ImmutableMap.of("id", spuId, "spuInfoMd5", spuInfoMd5));
   }

   public Long countOfValidSpu(Long categoryId) {
      Map<String, Object> params = Maps.newHashMap();
      params.put("categoryId", categoryId);
      Long count = (Long)this.getSqlSession().selectOne(this.sqlId("count"), params);
      return (Long)MoreObjects.firstNonNull(count, Long.valueOf(0L));
   }

   public void updateExtras(Long spuId, Map extras, String spuInfoMd5) {
      this.getSqlSession().update(this.sqlId("updateExtras"), ImmutableMap.of("spuId", spuId, "extraJson", JsonMapper.JSON_NON_DEFAULT_MAPPER.toJson(extras), "spuInfoMd5", spuInfoMd5));
   }
}
