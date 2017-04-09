package cn.blmdz.wolf.category.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.category.model.FrontCategory;

public interface FrontCategoryWriteService {
   Response create(FrontCategory var1);

   Response updateName(Long var1, String var2);

   Response updateLogo(Long var1, String var2);

   Response delete(Long var1);
}
