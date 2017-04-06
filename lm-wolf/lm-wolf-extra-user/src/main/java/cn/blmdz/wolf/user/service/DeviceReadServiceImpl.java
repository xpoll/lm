package cn.blmdz.wolf.user.service;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Optional;
import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.user.dao.UserDeviceDao;
import cn.blmdz.wolf.user.model.UserDevice;
import cn.blmdz.wolf.user.service.DeviceReadService;

@Service
public class DeviceReadServiceImpl implements DeviceReadService {
   private static final Logger log = LoggerFactory.getLogger(DeviceReadServiceImpl.class);
   private final UserDeviceDao userDeviceDao;

   @Autowired
   public DeviceReadServiceImpl(UserDeviceDao userDeviceDao) {
      this.userDeviceDao = userDeviceDao;
   }

   public Response findById(Long id) {
      try {
         return Response.ok(this.userDeviceDao.findById(id));
      } catch (Exception var3) {
         log.error("failed to find device by id = {}, cause : {}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("user.device.find.failed");
      }
   }

   public Response findByIds(List ids) {
      try {
         return Response.ok(this.userDeviceDao.findByIds(ids));
      } catch (Exception var3) {
         log.error("failed to find device by ids ({}), cause : {}", ids, Throwables.getStackTraceAsString(var3));
         return Response.fail("user.device.find.failed");
      }
   }

   public Response findByUserId(Long userId) {
      try {
         return Response.ok(this.userDeviceDao.findByUserId(userId));
      } catch (Exception var3) {
         log.error("failed to find device by userId = {}, cause : {}", userId, Throwables.getStackTraceAsString(var3));
         return Response.fail("user.device.find.failed");
      }
   }

   public Response findByDeviceToken(String deviceToken) {
      try {
         UserDevice device = this.userDeviceDao.findByDeviceToken(deviceToken);
         return Response.ok(Optional.fromNullable(device));
      } catch (Exception var3) {
         log.error("failed to find device by device token = {}, cause : {}", deviceToken, Throwables.getStackTraceAsString(var3));
         return Response.fail("user.device.find.failed");
      }
   }
}
