package io.terminus.parana.shop.service;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;
import java.util.List;

public interface ShopReadService {
   @Export(
      paramNames = {"shopId"}
   )
   Response findById(Long var1);

   Response findByIds(List var1);

   Response findByOuterId(String var1);

   Response findByUserId(Long var1);

   Response findByName(String var1);
}
