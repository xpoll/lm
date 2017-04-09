package cn.blmdz.home.session;

import java.util.Map;

import cn.blmdz.home.session.AFSession;

public interface SessionDataSource {
   Map<String, Object> findSessionById(String var1, String var2);

   void refreshExpireTime(AFSession var1, int var2);

   void refreshExpireTime(String var1, String var2, int var3);

   void deletePhysically(String var1, String var2);

   boolean save(String var1, String var2, Map<String, Object> var3, int var4);

   void destroy();
}
