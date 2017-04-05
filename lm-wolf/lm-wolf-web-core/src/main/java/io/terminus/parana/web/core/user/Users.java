package io.terminus.parana.web.core.user;

import com.google.common.eventbus.EventBus;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.pampas.common.UserUtil;
import io.terminus.parana.common.model.ParanaUser;
import io.terminus.parana.user.model.LoginType;
import io.terminus.parana.user.model.User;
import io.terminus.parana.user.service.UserReadService;
import io.terminus.parana.user.service.UserWriteService;
import io.terminus.parana.web.core.events.user.LoginEvent;
import io.terminus.parana.web.core.events.user.LogoutEvent;
import io.terminus.parana.web.core.events.user.RegisterEvent;
import io.terminus.parana.web.core.util.ParanaUserMaker;
import java.util.HashMap;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping({"/api/user"})
public class Users {
   private static final Logger log = LoggerFactory.getLogger(Users.class);
   private final UserWriteService userWriteService;
   private final UserReadService userReadService;
   private final EventBus eventBus;

   @Autowired
   public Users(UserWriteService userWriteService, UserReadService userReadService, EventBus eventBus) {
      this.userWriteService = userWriteService;
      this.userReadService = userReadService;
      this.eventBus = eventBus;
   }

   @RequestMapping({"/register"})
   public Long register(@RequestParam("loginId") String loginId, @RequestParam("loginType") int registerType, @RequestParam("password") String password) {
      User user = new User();
      user.setPassword(password);
      user.setStatus(Integer.valueOf(1));
      user.setType(Integer.valueOf(1));
      LoginType loginType = LoginType.from(registerType);
      switch(loginType) {
      case NAME:
         user.setName(loginId);
         break;
      case EMAIL:
         user.setEmail(loginId);
         break;
      case MOBILE:
         user.setMobile(loginId);
         break;
      default:
         log.error("unknown login register type({}) for user(loginId={})", Integer.valueOf(registerType), loginId);
         throw new JsonResponseException("unsupported login type");
      }

      Response<Long> rUser = this.userWriteService.create(user);
      if(!rUser.isSuccess()) {
         log.error("failed to register user(loginId={}, registerType={}), error code:{}", new Object[]{loginId, Integer.valueOf(registerType), rUser.getError()});
         throw new JsonResponseException(rUser.getError());
      } else {
         Long userId = (Long)rUser.getResult();
         user.setId(userId);
         this.eventBus.post(new RegisterEvent((HttpServletRequest)null, (HttpServletResponse)null, ParanaUserMaker.from(user)));
         return userId;
      }
   }

   @RequestMapping(
      value = {"/login"},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   @ResponseBody
   public Map login(@RequestParam("loginBy") String loginBy, @RequestParam("password") String password, @RequestParam(
   value = "target",
   required = false
) String target, @RequestParam(
   value = "type",
   required = false
) Integer type, HttpServletRequest request, HttpServletResponse response) {
      loginBy = loginBy.toLowerCase();
      LoginType loginType = Arguments.isNull(type)?LoginType.EMAIL:LoginType.from(type.intValue());
      Map<String, Object> map = new HashMap();
      Response<User> result = this.userReadService.login(loginBy, password, loginType);
      if(!result.isSuccess()) {
         log.warn("failed to login with(loginBy={}), error: {}", loginBy, result.getError());
         throw new JsonResponseException(500, result.getError());
      } else {
         User user = (User)result.getResult();
         if(user.getStatus().intValue() == 0) {
            log.warn("user({}) isn\'t active", user);
         }

         request.getSession().setAttribute("session_user_id", user.getId());
         LoginEvent loginEvent = new LoginEvent(request, response, ParanaUserMaker.from(user));
         this.eventBus.post(loginEvent);
         target = !StringUtils.hasText(target)?"/":target;
         map.put("redirect", target);
         return map;
      }
   }

   @RequestMapping(
      value = {"/logout"},
      method = {RequestMethod.GET}
   )
   public String logout(HttpServletRequest request, HttpServletResponse response) {
      try {
         HttpSession session = request.getSession(false);
         if(session != null) {
            session.invalidate();
         }

         ParanaUser loginUser = (ParanaUser)UserUtil.getCurrentUser();
         if(loginUser != null) {
            LogoutEvent logoutEvent = new LogoutEvent(request, response, loginUser);
            this.eventBus.post(logoutEvent);
         }

         return "redirect:/";
      } catch (Exception var6) {
         log.error("failed to logout user,cause:", var6);
         throw new JsonResponseException(500, "user.logout.fail");
      }
   }
}
