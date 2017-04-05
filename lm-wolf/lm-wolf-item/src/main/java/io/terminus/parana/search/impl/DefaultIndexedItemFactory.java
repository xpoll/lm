package io.terminus.parana.search.impl;

import io.terminus.parana.cache.BackCategoryCacher;
import io.terminus.parana.category.impl.dao.ShopCategoryItemDao;
import io.terminus.parana.search.impl.BaseIndexedItemFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class DefaultIndexedItemFactory extends BaseIndexedItemFactory {
   @Autowired
   public DefaultIndexedItemFactory(BackCategoryCacher backCategoryCacher, ShopCategoryItemDao shopCategoryItemDao) {
      super(backCategoryCacher, shopCategoryItemDao);
   }
}
