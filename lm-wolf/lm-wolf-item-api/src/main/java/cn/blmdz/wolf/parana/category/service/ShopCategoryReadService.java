package cn.blmdz.wolf.parana.category.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.parana.category.model.ShopCategory;

public interface ShopCategoryReadService {
   Response<ShopCategory> findById(Long var1);

   @Export(
      paramNames = {"shopId"}
   )
   Response findChildrenByShopId(Long var1);

   Response findChildrenByShopIdAndPid(Long var1, Long var2);

   @Export(
      paramNames = {"shopId"}
   )
   Response findEntireTreeByShopId(Long var1);
}
