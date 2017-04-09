package cn.blmdz.wolf.category.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;

public interface ShopCategoryItemReadService {
   Response findByShopIdAndItemId(Long var1, Long var2);

   Response findByShopIdAndItemIds(Long var1, List var2);

   Response findByShopIdAndCategoryId(Long var1, Long var2, Integer var3, Integer var4);
}
