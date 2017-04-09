package cn.blmdz.wolf.category.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.category.model.ShopCategoryItem;

public interface ShopCategoryItemWriteService {
   Response create(ShopCategoryItem var1);

   Response batchCreate(Long var1, List<Long> var2, List<Long> var3);

   Response delete(Long var1, Long var2, Long var3);

   Response batchDelete(Long var1, List<Long> var2, List<Long> var3);
}
