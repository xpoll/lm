package io.terminus.parana.category.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.category.model.CategoryBinding;
import java.util.List;
import org.springframework.stereotype.Repository;

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
