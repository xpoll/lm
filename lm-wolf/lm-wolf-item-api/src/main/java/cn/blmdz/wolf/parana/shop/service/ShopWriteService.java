package cn.blmdz.wolf.parana.shop.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.parana.shop.model.Shop;

public interface ShopWriteService {
   Response create(Shop var1);

   Response update(Shop var1);
}
