package io.terminus.parana.item.service;

import io.terminus.common.model.Response;
import java.util.List;

public interface SkuReadService {
   Response findSkuById(Long var1);

   Response findSkusByIds(List var1);

   Response findSkuByCode(Long var1, String var2);

   Response findSkusByItemId(Long var1);
}
