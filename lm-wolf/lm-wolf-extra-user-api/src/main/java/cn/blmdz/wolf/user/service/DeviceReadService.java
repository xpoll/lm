package cn.blmdz.wolf.user.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;

public interface DeviceReadService {
   Response findById(Long var1);

   Response findByIds(List var1);

   Response findByUserId(Long var1);

   Response findByDeviceToken(String var1);
}
