package cn.blmdz.wolf.parana.category.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;

public interface BackCategoryReadService {
   Response findById(Long var1);

   @Export(
      paramNames = {"pid"}
   )
   Response findChildrenByPid(Long var1);

   Response findAncestorsOf(Long var1);
}
