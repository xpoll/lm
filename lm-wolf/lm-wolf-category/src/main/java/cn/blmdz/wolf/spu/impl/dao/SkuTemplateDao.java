package cn.blmdz.wolf.spu.impl.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.spu.model.SkuTemplate;

@Repository
public class SkuTemplateDao extends MyBatisDao {
   public List findBySpuId(Long spuId) {
      return this.getSqlSession().selectList(this.sqlId("findBySpuId"), spuId);
   }

   public List findBySkuCode(String skuCode) {
      return this.getSqlSession().selectList(this.sqlId("findBySkuCode"), skuCode);
   }

   public SkuTemplate findBySpuIdAndSkuCode(Long spuId, String skuCode) {
      return (SkuTemplate)this.getSqlSession().selectOne(this.sqlId("findBySpuIdAndSkuCode"), ImmutableMap.of("spuId", spuId, "skuCode", skuCode));
   }

   public boolean updateBySpuIdAndSkuCode(SkuTemplate skuTemplate) {
      return this.getSqlSession().update(this.sqlId("updateBySpuIdAndSkuCode"), skuTemplate) == 1;
   }

   public void updateStatusBySpuId(Long spuId, Integer status) {
      this.getSqlSession().update(this.sqlId("updateStatusBySpuId"), ImmutableMap.of("spuId", spuId, "status", status));
   }

   public void deleteBySpuId(Long deletedSpuId) {
      this.getSqlSession().delete(this.sqlId("deleteBySpuId"), deletedSpuId);
   }
}
