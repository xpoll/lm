package io.terminus.parana.category.service;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;

public interface ShopCategoryReadService {
   Response findById(Long var1);

   @Export(
      paramNames = {"shopId"}
   )
   Response findChildrenByShopId(Long var1);

   Response findChildrenByShopIdAndPid(Long var1, Long var2);

   @Export(
      paramNames = {"shopId"}
   )
   Response findEntireTreeByShopId(Long var1);
}
