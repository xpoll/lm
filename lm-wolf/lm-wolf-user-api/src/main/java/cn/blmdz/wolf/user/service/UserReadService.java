package cn.blmdz.wolf.user.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.user.model.LoginType;
import cn.blmdz.wolf.user.model.User;

public interface UserReadService<T extends User> {
   Response<User> findById(Long var1);

   Response findByIds(List var1);

   Response<User> findBy(String var1, LoginType var2);

   Response login(String var1, String var2, LoginType var3);

   @Export(
      paramNames = {"id", "name", "email", "mobile", "status", "type", "pageNo", "pageSize"}
   )
   Response paging(Long var1, String var2, String var3, String var4, Integer var5, Integer var6, Integer var7, Integer var8);
}
