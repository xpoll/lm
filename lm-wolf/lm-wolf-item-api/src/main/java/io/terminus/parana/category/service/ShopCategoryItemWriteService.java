package io.terminus.parana.category.service;

import io.terminus.common.model.Response;
import io.terminus.parana.category.model.ShopCategoryItem;
import java.util.List;

public interface ShopCategoryItemWriteService {
   Response create(ShopCategoryItem var1);

   Response batchCreate(Long var1, List var2, List var3);

   Response delete(Long var1, Long var2, Long var3);

   Response batchDelete(Long var1, List var2, List var3);
}
