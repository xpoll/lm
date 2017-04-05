package io.terminus.parana.config.service;

import io.terminus.common.model.Response;

public interface ConfigReadService {
   Response list();

   Response findByKey(String var1);

   Response findByBizType(int var1);

   Response findByGroup(String var1);

   Response getUniqueConfig(int var1, String var2);
}
