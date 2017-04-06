package cn.blmdz.wolf.parana.category.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.parana.category.model.BackCategory;

public interface BackCategoryWriteService {
   Response create(BackCategory var1);

   Response updateName(Long var1, String var2);

   Response enable(Long var1);

   Response disable(Long var1);
}
