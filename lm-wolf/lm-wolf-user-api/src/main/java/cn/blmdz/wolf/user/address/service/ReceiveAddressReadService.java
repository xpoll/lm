package cn.blmdz.wolf.user.address.service;

import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;

public interface ReceiveAddressReadService {
   Response findAddressByUserId(Long var1);

   @Export(
      paramNames = {"baseUser"}
   )
   Response findAddressByLoginUser(BaseUser var1);

   Response findAddressById(Long var1);
}
