package io.terminus.parana.search.impl;

import io.terminus.parana.item.model.Item;
import io.terminus.parana.item.model.ItemAttribute;
import io.terminus.parana.search.dto.IndexedItem;

public interface IndexedItemFactory {
   IndexedItem create(Item var1, ItemAttribute var2, Object... var3);
}
