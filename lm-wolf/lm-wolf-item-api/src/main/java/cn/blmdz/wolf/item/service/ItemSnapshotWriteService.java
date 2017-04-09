package cn.blmdz.wolf.item.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.item.model.Item;
import cn.blmdz.wolf.item.model.ItemAttribute;
import cn.blmdz.wolf.item.model.ItemDetail;

public interface ItemSnapshotWriteService {
   Response create(Item var1, ItemDetail var2, ItemAttribute var3);
}
