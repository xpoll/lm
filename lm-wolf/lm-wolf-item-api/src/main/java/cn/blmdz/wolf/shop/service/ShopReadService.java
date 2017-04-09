package cn.blmdz.wolf.shop.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.shop.model.Shop;

public interface ShopReadService {
   @Export(
      paramNames = {"shopId"}
   )
   Response<Shop> findById(Long var1);

   Response findByIds(List var1);

   Response findByOuterId(String var1);

   Response<Shop> findByUserId(Long var1);

   Response findByName(String var1);
}
