package io.terminus.parana.category.service;

import io.terminus.common.model.Response;
import io.terminus.parana.category.model.FrontCategory;

public interface FrontCategoryWriteService {
   Response create(FrontCategory var1);

   Response updateName(Long var1, String var2);

   Response updateLogo(Long var1, String var2);

   Response delete(Long var1);
}
