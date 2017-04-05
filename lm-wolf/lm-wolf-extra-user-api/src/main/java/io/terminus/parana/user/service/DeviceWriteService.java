package io.terminus.parana.user.service;

import io.terminus.common.model.Response;
import io.terminus.parana.user.model.UserDevice;

public interface DeviceWriteService {
   Response create(UserDevice var1);

   Response update(UserDevice var1);

   Response delete(Long var1);

   Response deleteByDeviceToken(String var1);

   Response deleteByUserIdAndDeviceType(Long var1, String var2);
}
