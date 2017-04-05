package io.terminus.parana.user.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.BaseUser;
import io.terminus.common.model.Response;
import io.terminus.parana.user.dto.UserProfileDetail;
import io.terminus.parana.user.impl.dao.UserDao;
import io.terminus.parana.user.impl.dao.UserProfileDao;
import io.terminus.parana.user.model.User;
import io.terminus.parana.user.model.UserProfile;
import io.terminus.parana.user.service.UserProfileReadService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class UserProfileReadServiceImpl implements UserProfileReadService {
   private static final Logger log = LoggerFactory.getLogger(UserProfileReadServiceImpl.class);
   private UserProfileDao userProfileDao;
   private UserDao userDao;

   @Autowired
   public UserProfileReadServiceImpl(UserProfileDao userProfileDao, UserDao userDao) {
      this.userProfileDao = userProfileDao;
      this.userDao = userDao;
   }

   public Response findProfileByUser(BaseUser user) {
      Response<UserProfileDetail> resp = new Response();

      try {
         UserProfile profile = this.userProfileDao.findByUserId(user.getId());
         if(profile == null) {
            log.debug("user({})\'s profile isn\'t existed.", user);
            return Response.ok(new UserProfileDetail());
         }

         UserProfileDetail userProfileDetail = new UserProfileDetail();
         userProfileDetail.setProfile(profile);
         User loginUser = (User)this.userDao.findById(user.getId());
         userProfileDetail.setUser(loginUser);
         resp.setResult(userProfileDetail);
      } catch (Exception var6) {
         log.error("failed to find current login user({}), cause: {}", user, Throwables.getStackTraceAsString(var6));
         resp.setError("user.profile.find.fail");
      }

      return resp;
   }

   public Response findProfileByUserId(Long userId) {
      Response<UserProfile> resp = new Response();

      try {
         UserProfile profile = this.userProfileDao.findByUserId(userId);
         if(profile == null) {
            return Response.ok();
         }

         resp.setResult(profile);
      } catch (Exception var4) {
         log.error("failed to find current login user(id={}), cause: {}", userId, Throwables.getStackTraceAsString(var4));
         resp.setError("user.profile.find.fail");
      }

      return resp;
   }
}
