package io.terminus.parana.web.admin.interceptors;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import io.terminus.common.model.Response;
import io.terminus.pampas.common.UserUtil;
import io.terminus.parana.common.model.ParanaUser;
import io.terminus.parana.user.model.User;
import io.terminus.parana.user.service.UserReadService;
import io.terminus.parana.web.core.util.ParanaUserMaker;
import java.util.concurrent.TimeUnit;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

@Component
public class ParanaLoginInterceptor extends HandlerInterceptorAdapter {
   private static final Logger log = LoggerFactory.getLogger(ParanaLoginInterceptor.class);
   private final LoadingCache userCache;

   @Autowired
   public ParanaLoginInterceptor(final UserReadService userReadService) {
      this.userCache = CacheBuilder.newBuilder().expireAfterWrite(30L, TimeUnit.MINUTES).build(new CacheLoader() {
         public Response load(Long userId) throws Exception {
            return userReadService.findById(userId);
         }
      });
   }

   public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
      HttpSession session = request.getSession(false);
      if(session != null) {
         Object userIdInSession = session.getAttribute("session_user_id");
         if(userIdInSession != null) {
            Long userId = Long.valueOf(userIdInSession.toString());
            Response<? extends User> result = (Response)this.userCache.getUnchecked(userId);
            if(!result.isSuccess()) {
               log.warn("failed to find user where id={},error code:{}", userId, result.getError());
               return false;
            }

            User user = (User)result.getResult();
            if(user != null) {
               ParanaUser paranaUser = ParanaUserMaker.from(user);
               if(user.getType().intValue() != 1) {
                  log.error("user(id={})\'s is not admin, its type is {}", userId, user.getType());
               }

               UserUtil.putCurrentUser(paranaUser);
            }
         }
      }

      return true;
   }

   public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object handler, Exception ex) throws Exception {
      UserUtil.clearCurrentUser();
   }
}
