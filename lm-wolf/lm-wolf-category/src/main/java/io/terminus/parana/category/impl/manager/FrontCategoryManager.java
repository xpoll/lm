package io.terminus.parana.category.impl.manager;

import io.terminus.parana.category.impl.dao.CategoryBindingDao;
import io.terminus.parana.category.impl.dao.FrontCategoryDao;
import io.terminus.parana.category.model.FrontCategory;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
public class FrontCategoryManager {
   @Autowired
   private FrontCategoryDao frontCategoryDao;
   @Autowired
   private CategoryBindingDao categoryBindingDao;

   @Transactional
   public void create(FrontCategory frontCategory, FrontCategory parent) {
      this.frontCategoryDao.create(frontCategory);
      this.frontCategoryDao.updateHasChildren(parent.getId(), Boolean.TRUE);
   }

   @Transactional
   public void delete(Long categoryId, Long parentId) {
      this.frontCategoryDao.delete(categoryId);
      this.categoryBindingDao.deleteByFrontCategoryId(categoryId);
      if(parentId.longValue() > 0L) {
         List<FrontCategory> children = this.frontCategoryDao.findChildren(parentId);
         if(children.isEmpty()) {
            this.frontCategoryDao.updateHasChildren(parentId, Boolean.FALSE);
         }
      }

   }
}
