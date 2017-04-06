package cn.blmdz.wolf.category.impl.manager;

import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import cn.blmdz.wolf.category.impl.dao.ShopCategoryDao;
import cn.blmdz.wolf.category.impl.dao.ShopCategoryItemDao;
import cn.blmdz.wolf.parana.category.model.ShopCategory;

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
   public void batchUpdate(List<ShopCategory> toUpdates) {
      for(ShopCategory toUpdate : toUpdates) {
         this.shopCategoryDao.update(toUpdate);
      }

   }
}
