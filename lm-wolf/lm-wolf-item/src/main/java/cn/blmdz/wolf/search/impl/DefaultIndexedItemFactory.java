package cn.blmdz.wolf.search.impl;

import org.springframework.beans.factory.annotation.Autowired;

import cn.blmdz.wolf.cache.BackCategoryCacher;
import cn.blmdz.wolf.category.impl.dao.ShopCategoryItemDao;
import cn.blmdz.wolf.search.dto.IndexedItem;

public class DefaultIndexedItemFactory extends BaseIndexedItemFactory<IndexedItem>
{
  @Autowired
  public DefaultIndexedItemFactory(BackCategoryCacher backCategoryCacher, ShopCategoryItemDao shopCategoryItemDao)
  {
    super(backCategoryCacher, shopCategoryItemDao);
  }
}