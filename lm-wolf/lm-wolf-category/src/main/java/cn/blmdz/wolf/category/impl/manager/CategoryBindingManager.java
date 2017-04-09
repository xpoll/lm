package cn.blmdz.wolf.category.impl.manager;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import cn.blmdz.wolf.category.impl.dao.CategoryBindingDao;
import cn.blmdz.wolf.category.impl.dao.FrontCategoryDao;
import cn.blmdz.wolf.category.model.CategoryBinding;
import cn.blmdz.wolf.category.model.FrontCategory;

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
   public void multiBind(List<CategoryBinding> categoryBindings) {
      for(CategoryBinding categoryBinding : categoryBindings) {
         this.categoryBindingDao.create(categoryBinding);
      }

   }
}
