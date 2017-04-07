package cn.blmdz.wolf.search.impl.shop.impl;

import cn.blmdz.wolf.parana.search.dto.IndexedShop;
import cn.blmdz.wolf.parana.shop.model.Shop;

public abstract interface IndexedShopFactory<T extends IndexedShop>
{
  public abstract T create(Shop paramShop, Object[] paramArrayOfObject);
}