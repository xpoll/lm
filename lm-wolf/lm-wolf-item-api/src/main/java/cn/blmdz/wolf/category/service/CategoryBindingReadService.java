package cn.blmdz.wolf.category.service;

import cn.blmdz.home.common.model.Response;

public interface CategoryBindingReadService {
   Response findByFrontCategoryId(Long var1);

   Response findByBackCategoryId(Long var1);
}
