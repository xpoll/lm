package cn.blmdz.wolf.web.front.interceptors;

import java.util.concurrent.TimeUnit;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import com.google.common.base.Objects;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.common.utils.Iters;
import cn.blmdz.wolf.parana.shop.model.Shop;
import cn.blmdz.wolf.parana.shop.service.ShopReadService;
import cn.blmdz.wolf.user.model.User;
import cn.blmdz.wolf.user.service.UserReadService;
import cn.blmdz.wolf.web.core.util.ParanaUserMaker;

@Component
public class ParanaLoginInterceptor extends HandlerInterceptorAdapter {
   private static final Logger log = LoggerFactory.getLogger(ParanaLoginInterceptor.class);
   private final LoadingCache<Long, Response> userCache;
   private final LoadingCache<Long, Response> shopCache;

   @Autowired
   public ParanaLoginInterceptor(final UserReadService userReadService, final ShopReadService shopReadService) {
      this.userCache = CacheBuilder.newBuilder().expireAfterWrite(30L, TimeUnit.MINUTES).build(new CacheLoader<Long, Response>() {
         public Response load(Long userId) throws Exception {
            return userReadService.findById(userId);
         }
      });
      this.shopCache = CacheBuilder.newBuilder().expireAfterWrite(30L, TimeUnit.MINUTES).build(new CacheLoader<Long, Response>() {
         public Response load(Long userId) throws Exception {
            return shopReadService.findByUserId(userId);
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
               if(Objects.equal(user.getType(), Integer.valueOf(2)) && Iters.nullToEmpty(user.getRoles()).contains("SELLER")) {
                  Response<Shop> rShop = (Response)this.shopCache.getUnchecked(userId);
                  if(!rShop.isSuccess()) {
                     log.error("failed to find shop for user(id={}), error code:{}", user.getId(), rShop.getError());
                  } else {
                     Shop shop = (Shop)rShop.getResult();
                     paranaUser.setShopId(shop.getId());
                  }
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
