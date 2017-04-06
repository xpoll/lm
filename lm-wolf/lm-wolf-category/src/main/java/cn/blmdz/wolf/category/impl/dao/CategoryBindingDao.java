package cn.blmdz.wolf.category.impl.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.parana.category.model.CategoryBinding;

@Repository
public class CategoryBindingDao extends MyBatisDao {
   public List findByBackCategoryId(Long backCategoryId) {
      return this.getSqlSession().selectList(this.sqlId("findByBackCategoryId"), backCategoryId);
   }

   public List findByFrontCategoryId(Long frontCategoryId) {
      return this.getSqlSession().selectList(this.sqlId("findByFrontCategoryId"), frontCategoryId);
   }

   public CategoryBinding findByFrontBackCategoryId(Long frontCategoryId, Long backCategoryId) {
      return (CategoryBinding)this.getSqlSession().selectOne(this.sqlId("findByFrontBackCategoryId"), ImmutableMap.of("frontCategoryId", frontCategoryId, "backCategoryId", backCategoryId));
   }

   public void deleteByFrontCategoryId(Long frontCategoryId) {
      this.getSqlSession().delete(this.sqlId("deleteByFrontCategoryId"), frontCategoryId);
   }

   public void deleteByBackCategoryId(Long backCategoryId) {
      this.getSqlSession().delete(this.sqlId("deleteByBackCategoryId"), backCategoryId);
   }
}
