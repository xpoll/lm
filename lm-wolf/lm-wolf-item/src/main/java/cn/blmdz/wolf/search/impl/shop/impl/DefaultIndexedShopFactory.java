package cn.blmdz.wolf.search.impl.shop.impl;

import cn.blmdz.wolf.item.impl.dao.ItemDao;
import cn.blmdz.wolf.parana.search.dto.IndexedShop;
import cn.blmdz.wolf.parana.shop.model.Shop;

public class DefaultIndexedShopFactory extends BaseIndexedShopFactory<IndexedShop>
{
  private final ItemDao itemDao;

  public DefaultIndexedShopFactory(ItemDao itemDao)
  {
    this.itemDao = itemDao;
  }

  public IndexedShop create(Shop shop, Object[] others)
  {
    IndexedShop indexedShop = super.create(shop, others);

    long itemCount = this.itemDao.countOnShelfByShopId(shop.getId());
    indexedShop.setItemCount(itemCount);
    return indexedShop;
  }
}