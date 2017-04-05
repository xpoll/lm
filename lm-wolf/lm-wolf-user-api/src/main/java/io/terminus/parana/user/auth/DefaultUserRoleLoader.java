package io.terminus.parana.user.auth;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.user.auth.UserRoleLoader;
import io.terminus.parana.user.model.User;
import io.terminus.parana.user.service.UserReadService;
import java.util.Collections;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DefaultUserRoleLoader implements UserRoleLoader {
   private static final Logger log = LoggerFactory.getLogger(DefaultUserRoleLoader.class);
   private final UserReadService userReadService;

   public DefaultUserRoleLoader(UserReadService userReadService) {
      this.userReadService = userReadService;
   }

   public Response hardLoadRoles(Long userId) {
      try {
         if(userId == null) {
            log.warn("hard load roles failed, userId=null");
            return Response.fail("user.id.null");
         } else {
            Response<User> findResp = this.userReadService.findById(userId);
            if(!findResp.isSuccess()) {
               log.warn("find user failed, userId={}, error={}", userId, findResp.getError());
               return Response.fail(findResp.getError());
            } else {
               User user = (User)findResp.getResult();
               if(user == null) {
                  log.warn("hard load roles failed, user not found, id={}", userId);
                  return Response.fail("user.not.found");
               } else {
                  List<String> result = user.getRoles() == null?Collections.emptyList():user.getRoles();
                  return Response.ok(result);
               }
            }
         }
      } catch (Exception var5) {
         log.error("hard load roles failed, userId={}, cause:{}", userId, Throwables.getStackTraceAsString(var5));
         return Response.fail("user.role.load.fail");
      }
   }
}
