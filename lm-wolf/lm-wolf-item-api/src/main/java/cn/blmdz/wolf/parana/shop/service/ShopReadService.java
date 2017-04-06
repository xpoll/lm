package cn.blmdz.wolf.parana.shop.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;

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
