package cn.blmdz.wolf.category.impl.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.base.MoreObjects;
import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.category.model.CategoryAttribute;

@Repository
public class CategoryAttributeDao extends MyBatisDao {
   public List findByCategoryId(Long categoryId) {
      return this.getSqlSession().selectList(this.sqlId("findByCategoryId"), categoryId);
   }

   public boolean updateStatus(Long id, Integer status) {
      return this.getSqlSession().update(this.sqlId("updateStatus"), ImmutableMap.of("id", id, "status", status)) == 1;
   }

   public boolean updateIndex(Long id, Integer index) {
      return this.getSqlSession().update(this.sqlId("updateIndex"), ImmutableMap.of("id", id, "index", index)) == 1;
   }

   public CategoryAttribute findByCategoryIdAndAttrKey(Long categoryId, String attrKey) {
      return (CategoryAttribute)this.getSqlSession().selectOne(this.sqlId("findByCategoryIdAndAttrKey"), ImmutableMap.of("categoryId", categoryId, "attrKey", attrKey));
   }

   public Integer maxIndexOfCategoryId(Long categoryId) {
      Integer maxIndexId = (Integer)this.getSqlSession().selectOne(this.sqlId("categoryMaxIndexId"), categoryId);
      return (Integer)MoreObjects.firstNonNull(maxIndexId, Integer.valueOf(0));
   }

   public Integer countOfValidCategoryAttribute(Long categoryId) {
      Integer count = (Integer)this.getSqlSession().selectOne(this.sqlId("count"), categoryId);
      return (Integer)MoreObjects.firstNonNull(count, Integer.valueOf(0));
   }
}
