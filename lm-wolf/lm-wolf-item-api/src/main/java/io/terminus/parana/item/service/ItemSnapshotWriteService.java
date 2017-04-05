package io.terminus.parana.item.service;

import io.terminus.common.model.Response;
import io.terminus.parana.item.model.Item;
import io.terminus.parana.item.model.ItemAttribute;
import io.terminus.parana.item.model.ItemDetail;

public interface ItemSnapshotWriteService {
   Response create(Item var1, ItemDetail var2, ItemAttribute var3);
}
