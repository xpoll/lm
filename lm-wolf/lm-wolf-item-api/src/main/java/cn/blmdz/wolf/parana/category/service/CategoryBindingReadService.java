package cn.blmdz.wolf.parana.category.service;

import cn.blmdz.home.common.model.Response;

public interface CategoryBindingReadService {
   Response findByFrontCategoryId(Long var1);

   Response findByBackCategoryId(Long var1);
}
