package io.terminus.parana.user.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.user.impl.dao.UserProfileDao;
import io.terminus.parana.user.model.UserProfile;
import io.terminus.parana.user.service.UserProfileWriteService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

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
