package io.terminus.parana.category.impl.manager;

import io.terminus.parana.category.impl.dao.CategoryBindingDao;
import io.terminus.parana.category.impl.dao.FrontCategoryDao;
import io.terminus.parana.category.model.CategoryBinding;
import io.terminus.parana.category.model.FrontCategory;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

@Component
public class CategoryBindingManager {
   private final FrontCategoryDao frontCategoryDao;
   private final CategoryBindingDao categoryBindingDao;

   @Autowired
   public CategoryBindingManager(FrontCategoryDao frontCategoryDao, CategoryBindingDao categoryBindingDao) {
      this.frontCategoryDao = frontCategoryDao;
      this.categoryBindingDao = categoryBindingDao;
   }

   @Transactional
   public void deleteFrontCategory(FrontCategory frontCategory) {
      Long id = frontCategory.getId();
      this.frontCategoryDao.delete(id);
      this.categoryBindingDao.deleteByFrontCategoryId(id);
      Long pid = frontCategory.getPid();
      List<FrontCategory> siblings = this.frontCategoryDao.findChildren(pid);
      if(CollectionUtils.isEmpty(siblings)) {
         this.frontCategoryDao.updateHasChildren(pid, Boolean.valueOf(false));
      }

   }

   @Transactional
   public void multiBind(List categoryBindings) {
      for(CategoryBinding categoryBinding : categoryBindings) {
         this.categoryBindingDao.create(categoryBinding);
      }

   }
}
