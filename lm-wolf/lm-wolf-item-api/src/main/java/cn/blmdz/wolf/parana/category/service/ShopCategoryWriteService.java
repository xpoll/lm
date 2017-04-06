package cn.blmdz.wolf.parana.category.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.parana.category.model.ShopCategory;

public interface ShopCategoryWriteService {
   Response create(ShopCategory var1);

   Response updateName(Long var1, Long var2, String var3);

   Response delete(Long var1, Long var2);

   Response move(Long var1, int var2);

   Response updateDisclosed(Long var1, Boolean var2);
}
