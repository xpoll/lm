package cn.blmdz.wolf.user.service;

import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;

public interface UserProfileReadService {
   @Export(
      paramNames = {"user"}
   )
   Response findProfileByUser(BaseUser var1);

   @Export(
      paramNames = {"userId"}
   )
   Response findProfileByUserId(Long var1);
}
