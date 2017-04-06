package cn.blmdz.wolf.web.core.events.user;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.web.core.events.user.UserEvent;

public class RegisterEvent extends UserEvent {
   public RegisterEvent(HttpServletRequest request, HttpServletResponse response, ParanaUser user) {
      super(request, response, user);
   }

   public RegisterEvent(HttpServletRequest request, HttpServletResponse response) {
      super(request, response);
   }
}
