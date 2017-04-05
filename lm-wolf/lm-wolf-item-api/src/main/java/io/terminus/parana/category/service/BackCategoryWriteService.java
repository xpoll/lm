package io.terminus.parana.category.service;

import io.terminus.common.model.Response;
import io.terminus.parana.category.model.BackCategory;

public interface BackCategoryWriteService {
   Response create(BackCategory var1);

   Response updateName(Long var1, String var2);

   Response enable(Long var1);

   Response disable(Long var1);
}
