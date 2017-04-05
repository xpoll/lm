package io.terminus.parana.brand.service;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;
import java.util.List;

public interface BrandReadService {
   Response findById(Long var1);

   @Export(
      paramNames = {"brandIds"}
   )
   Response findByIds(List var1);

   Response findByName(String var1);

   Response exist(String var1);

   Response findByNamePrefix(String var1, Integer var2);

   @Export(
      paramNames = {"pageNo", "pageSize", "namePrefix"}
   )
   Response pagination(Integer var1, Integer var2, String var3);
}
