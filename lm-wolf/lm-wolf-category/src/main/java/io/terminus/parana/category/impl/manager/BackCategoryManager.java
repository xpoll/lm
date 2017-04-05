package io.terminus.parana.category.impl.manager;

import io.terminus.parana.category.impl.dao.BackCategoryDao;
import io.terminus.parana.category.impl.dao.CategoryBindingDao;
import io.terminus.parana.category.model.BackCategory;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
public class BackCategoryManager {
   private static final Logger log = LoggerFactory.getLogger(BackCategoryManager.class);
   @Autowired
   private BackCategoryDao backCategoryDao;
   @Autowired
   private CategoryBindingDao categoryBindingDao;

   @Transactional
   public void create(BackCategory backCategory, BackCategory parent) {
      this.backCategoryDao.create(backCategory);
      parent.setHasChildren(Boolean.valueOf(true));
      this.backCategoryDao.update(parent);
   }

   @Transactional
   public void disable(Long categoryId, Long parentId) {
      this.backCategoryDao.updateStatusById(categoryId, Integer.valueOf(-1));
      this.categoryBindingDao.deleteByBackCategoryId(categoryId);
      if(parentId.longValue() > 0L) {
         List<BackCategory> children = this.backCategoryDao.findChildren(parentId);
         if(children.isEmpty()) {
            BackCategory parent = new BackCategory();
            parent.setId(parentId);
            parent.setHasChildren(Boolean.valueOf(false));
            this.backCategoryDao.update(parent);
         }
      }

   }

   @Transactional
   public void enable(BackCategory backCategory) {
      this.backCategoryDao.updateStatusById(backCategory.getId(), Integer.valueOf(1));
      Long parentId = backCategory.getPid();
      if(parentId.longValue() > 0L) {
         List<BackCategory> children = this.backCategoryDao.findChildren(parentId);
         if(!children.isEmpty()) {
            BackCategory parent = new BackCategory();
            parent.setId(parentId);
            parent.setHasChildren(Boolean.valueOf(true));
            this.backCategoryDao.update(parent);
         }
      }

   }
}
