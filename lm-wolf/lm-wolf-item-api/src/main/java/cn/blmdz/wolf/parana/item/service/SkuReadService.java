package cn.blmdz.wolf.parana.item.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;

public interface SkuReadService {
   Response findSkuById(Long var1);

   Response findSkusByIds(List var1);

   Response findSkuByCode(Long var1, String var2);

   Response findSkusByItemId(Long var1);
}
