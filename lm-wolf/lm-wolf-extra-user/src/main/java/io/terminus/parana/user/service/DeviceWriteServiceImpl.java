package io.terminus.parana.user.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.user.dao.UserDeviceDao;
import io.terminus.parana.user.model.UserDevice;
import io.terminus.parana.user.service.DeviceWriteService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class DeviceWriteServiceImpl implements DeviceWriteService {
   private static final Logger log = LoggerFactory.getLogger(DeviceWriteServiceImpl.class);
   private final UserDeviceDao userDeviceDao;

   @Autowired
   public DeviceWriteServiceImpl(UserDeviceDao userDeviceDao) {
      this.userDeviceDao = userDeviceDao;
   }

   public Response create(UserDevice userDevice) {
      try {
         this.userDeviceDao.create(userDevice);
         return Response.ok(userDevice.getId());
      } catch (Exception var3) {
         log.error("failed to create UserDevice = ({}), cause : {}", userDevice, Throwables.getStackTraceAsString(var3));
         return Response.fail("user.device.create.failed");
      }
   }

   public Response update(UserDevice userDevice) {
      try {
         return Response.ok(this.userDeviceDao.update(userDevice));
      } catch (Exception var3) {
         log.error("failed to update UserDevice = ({}), cause : {}", userDevice, Throwables.getStackTraceAsString(var3));
         return Response.fail("user.device.update.failed");
      }
   }

   public Response delete(Long id) {
      try {
         return Response.ok(this.userDeviceDao.delete(id));
      } catch (Exception var3) {
         log.error("failed to delete user device by id = ({}), cause : {}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("user.device.delete.failed");
      }
   }

   public Response deleteByDeviceToken(String deviceToken) {
      try {
         return Response.ok(this.userDeviceDao.deleteByDeviceToken(deviceToken));
      } catch (Exception var3) {
         log.error("failed to delete user device by device token = ({}), cause : {}", deviceToken, Throwables.getStackTraceAsString(var3));
         return Response.fail("user.device.delete.failed");
      }
   }

   public Response deleteByUserIdAndDeviceType(Long userId, String deviceType) {
      try {
         return Response.ok(this.userDeviceDao.deleteByUserIdAndDeviceType(userId, deviceType));
      } catch (Exception var4) {
         log.error("failed to delete user device by userId = ({})device type = ({}), cause : {}", new Object[]{userId, deviceType, Throwables.getStackTraceAsString(var4)});
         return Response.fail("user.device.delete.failed");
      }
   }
}
