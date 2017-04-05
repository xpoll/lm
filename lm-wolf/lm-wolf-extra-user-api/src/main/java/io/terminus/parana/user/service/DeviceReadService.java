package io.terminus.parana.user.service;

import io.terminus.common.model.Response;
import java.util.List;

public interface DeviceReadService {
   Response findById(Long var1);

   Response findByIds(List var1);

   Response findByUserId(Long var1);

   Response findByDeviceToken(String var1);
}
