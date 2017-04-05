package io.terminus.parana.item.service;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;

public interface AdminItemReadService {
   @Export(
      paramNames = {"shopId", "pageNo", "pageSize"}
   )
   Response findByShopId(Long var1, Integer var2, Integer var3);

   @Export(
      paramNames = {"userId", "pageNo", "pageSize"}
   )
   Response findByUserId(Long var1, Integer var2, Integer var3);

   @Export(
      paramNames = {"itemId", "userId", "shopId", "itemName", "pageNo", "pageSize"}
   )
   Response findBy(Long var1, Long var2, Long var3, String var4, Integer var5, Integer var6);
}
