package cn.blmdz.wolf.user.impl.service;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.user.impl.dao.UserDao;
import cn.blmdz.wolf.user.service.AdminUserService;

@Service
public class AdminUserServiceImpl implements AdminUserService {
   private static final Logger log = LoggerFactory.getLogger(AdminUserServiceImpl.class);
   private final UserDao userDao;

   @Autowired
   public AdminUserServiceImpl(UserDao userDao) {
      this.userDao = userDao;
   }

   public Response updateTags(Long userId, Map tags) {
      try {
         this.userDao.updateTags(userId, tags);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("failed to update tags to {} for user(id={}), cause:{}", new Object[]{tags, userId, Throwables.getStackTraceAsString(var4)});
         return Response.fail("user.tags.update.fail");
      }
   }

   public Response updateStatus(Long userId, Integer status) {
      try {
         this.userDao.updateStatus(userId, status);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("failed to update status to {} for user(id={}), cause:{}", new Object[]{status, userId, Throwables.getStackTraceAsString(var4)});
         return Response.fail("user.status.update.fail");
      }
   }
}
