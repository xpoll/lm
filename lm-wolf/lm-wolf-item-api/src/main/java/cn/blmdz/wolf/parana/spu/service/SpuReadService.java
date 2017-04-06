package cn.blmdz.wolf.parana.spu.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;

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
