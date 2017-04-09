package cn.blmdz.wolf.shop.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.shop.model.Shop;

public interface ShopWriteService {
   Response<Long> create(Shop var1);

   Response update(Shop var1);
}
