package io.terminus.parana.user.service;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;
import io.terminus.parana.user.model.LoginType;
import java.util.List;

public interface UserReadService {
   Response findById(Long var1);

   Response findByIds(List var1);

   Response findBy(String var1, LoginType var2);

   Response login(String var1, String var2, LoginType var3);

   @Export(
      paramNames = {"id", "name", "email", "mobile", "status", "type", "pageNo", "pageSize"}
   )
   Response paging(Long var1, String var2, String var3, String var4, Integer var5, Integer var6, Integer var7, Integer var8);
}
