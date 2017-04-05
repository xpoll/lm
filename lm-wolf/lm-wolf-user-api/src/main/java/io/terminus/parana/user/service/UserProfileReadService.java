package io.terminus.parana.user.service;

import io.terminus.common.model.BaseUser;
import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;

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
