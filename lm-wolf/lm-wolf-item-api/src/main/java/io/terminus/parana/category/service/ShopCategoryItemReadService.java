package io.terminus.parana.category.service;

import io.terminus.common.model.Response;
import java.util.List;

public interface ShopCategoryItemReadService {
   Response findByShopIdAndItemId(Long var1, Long var2);

   Response findByShopIdAndItemIds(Long var1, List var2);

   Response findByShopIdAndCategoryId(Long var1, Long var2, Integer var3, Integer var4);
}
