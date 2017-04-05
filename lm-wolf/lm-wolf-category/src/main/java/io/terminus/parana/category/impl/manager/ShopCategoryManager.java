package io.terminus.parana.category.impl.manager;

import io.terminus.parana.category.impl.dao.ShopCategoryDao;
import io.terminus.parana.category.impl.dao.ShopCategoryItemDao;
import io.terminus.parana.category.model.ShopCategory;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
public class ShopCategoryManager {
   private final ShopCategoryDao shopCategoryDao;
   private final ShopCategoryItemDao shopCategoryItemDao;

   @Autowired
   public ShopCategoryManager(ShopCategoryDao shopCategoryDao, ShopCategoryItemDao shopCategoryItemDao) {
      this.shopCategoryDao = shopCategoryDao;
      this.shopCategoryItemDao = shopCategoryItemDao;
   }

   @Transactional
   public void create(ShopCategory shopCategory, ShopCategory parent) {
      this.shopCategoryDao.create(shopCategory);
      this.shopCategoryDao.update(parent);
   }

   @Transactional
   public void delete(ShopCategory shopCategory, ShopCategory parent) {
      this.shopCategoryDao.delete(shopCategory.getId());
      this.shopCategoryItemDao.deleteByShopIdAndCategoryId(shopCategory.getShopId(), shopCategory.getId());
      if(parent != null) {
         this.shopCategoryDao.update(parent);
      }

   }

   @Transactional
   public void batchUpdate(List toUpdates) {
      for(ShopCategory toUpdate : toUpdates) {
         this.shopCategoryDao.update(toUpdate);
      }

   }
}
