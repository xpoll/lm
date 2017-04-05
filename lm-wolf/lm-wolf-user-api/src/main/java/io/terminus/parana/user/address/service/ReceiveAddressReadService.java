package io.terminus.parana.user.address.service;

import io.terminus.common.model.BaseUser;
import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;

public interface ReceiveAddressReadService {
   Response findAddressByUserId(Long var1);

   @Export(
      paramNames = {"baseUser"}
   )
   Response findAddressByLoginUser(BaseUser var1);

   Response findAddressById(Long var1);
}
