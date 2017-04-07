package cn.blmdz.wolf.search.impl;

import cn.blmdz.wolf.parana.item.model.Item;
import cn.blmdz.wolf.parana.item.model.ItemAttribute;
import cn.blmdz.wolf.parana.search.dto.IndexedItem;

public interface IndexedItemFactory<T> {
   T create(Item var1, ItemAttribute var2, Object... var3);
}
