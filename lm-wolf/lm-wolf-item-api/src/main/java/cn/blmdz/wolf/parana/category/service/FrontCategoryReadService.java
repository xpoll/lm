package cn.blmdz.wolf.parana.category.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;

public interface FrontCategoryReadService {
   @Export(
      paramNames = {"pid"}
   )
   Response findChildrenByPid(Long var1);

   Response findById(Long var1);

   Response findByIds(List var1);
}
