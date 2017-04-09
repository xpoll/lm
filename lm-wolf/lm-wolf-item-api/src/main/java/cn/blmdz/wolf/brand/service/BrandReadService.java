package cn.blmdz.wolf.brand.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;

public interface BrandReadService {
   Response findById(Long var1);

   @Export(
      paramNames = {"brandIds"}
   )
   Response findByIds(List var1);

   Response findByName(String var1);

   Response exist(String var1);

   Response findByNamePrefix(String var1, Integer var2);

   @Export(
      paramNames = {"pageNo", "pageSize", "namePrefix"}
   )
   Response pagination(Integer var1, Integer var2, String var3);
}
