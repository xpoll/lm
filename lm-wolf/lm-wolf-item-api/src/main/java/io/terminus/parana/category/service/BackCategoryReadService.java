package io.terminus.parana.category.service;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;

public interface BackCategoryReadService {
   Response findById(Long var1);

   @Export(
      paramNames = {"pid"}
   )
   Response findChildrenByPid(Long var1);

   Response findAncestorsOf(Long var1);
}
