package cn.blmdz.wolf.search.impl;

import cn.blmdz.wolf.item.model.Item;
import cn.blmdz.wolf.item.model.ItemAttribute;
import cn.blmdz.wolf.search.dto.IndexedItem;

public interface IndexedItemFactory<T> {
   T create(Item var1, ItemAttribute var2, Object... var3);
}
