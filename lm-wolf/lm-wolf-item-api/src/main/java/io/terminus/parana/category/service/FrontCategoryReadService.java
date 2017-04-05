package io.terminus.parana.category.service;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;
import java.util.List;

public interface FrontCategoryReadService {
   @Export(
      paramNames = {"pid"}
   )
   Response findChildrenByPid(Long var1);

   Response findById(Long var1);

   Response findByIds(List var1);
}
