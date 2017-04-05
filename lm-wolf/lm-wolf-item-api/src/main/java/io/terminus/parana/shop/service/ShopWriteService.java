package io.terminus.parana.shop.service;

import io.terminus.common.model.Response;
import io.terminus.parana.shop.model.Shop;

public interface ShopWriteService {
   Response create(Shop var1);

   Response update(Shop var1);
}
