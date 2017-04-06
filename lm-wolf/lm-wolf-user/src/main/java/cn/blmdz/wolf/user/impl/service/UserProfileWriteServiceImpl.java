package cn.blmdz.wolf.user.impl.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.user.impl.dao.UserProfileDao;
import cn.blmdz.wolf.user.model.UserProfile;
import cn.blmdz.wolf.user.service.UserProfileWriteService;

@Service
public class UserProfileWriteServiceImpl implements UserProfileWriteService {
   private static final Logger log = LoggerFactory.getLogger(UserProfileWriteServiceImpl.class);
   @Autowired
   private UserProfileDao userProfileDao;

   public Response updateProfile(UserProfile profile) {
      Response<Boolean> resp = new Response();

      try {
         this.userProfileDao.update(profile);
         resp.setResult(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("failed to update profile({}), cause: {}", profile, Throwables.getStackTraceAsString(var4));
         resp.setError("user.profile.update.fail");
      }

      return resp;
   }

   public Response createProfile(UserProfile profile) {
      Response<Boolean> resp = new Response();

      try {
         this.userProfileDao.create(profile);
         resp.setResult(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("failed to update profile({}), cause: {}", profile, Throwables.getStackTraceAsString(var4));
         resp.setError("user.profile.update.fail");
      }

      return resp;
   }

   public void setUserProfileDao(UserProfileDao userProfileDao) {
      this.userProfileDao = userProfileDao;
   }
}
