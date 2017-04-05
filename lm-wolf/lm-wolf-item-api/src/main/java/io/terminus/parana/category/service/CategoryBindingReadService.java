package io.terminus.parana.category.service;

import io.terminus.common.model.Response;

public interface CategoryBindingReadService {
   Response findByFrontCategoryId(Long var1);

   Response findByBackCategoryId(Long var1);
}
