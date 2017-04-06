package cn.blmdz.wolf.user.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.user.model.UserDevice;

public interface DeviceWriteService {
   Response create(UserDevice var1);

   Response update(UserDevice var1);

   Response delete(Long var1);

   Response deleteByDeviceToken(String var1);

   Response deleteByUserIdAndDeviceType(Long var1, String var2);
}
