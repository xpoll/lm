package io.terminus.parana.spu.service;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;

public interface SpuReadService {
   @Export(
      paramNames = {"categoryId", "keyword", "pageNo", "pageSize"}
   )
   Response findByCategoryId(Long var1, String var2, Integer var3, Integer var4);

   @Export(
      paramNames = {"spuId"}
   )
   Response findFullInfoBySpuId(Long var1);

   Response findById(Long var1);

   @Export(
      paramNames = {"spuId"}
   )
   Response findRichTextById(Long var1);
}
