package io.terminus.parana.web.core.events.user;

import io.terminus.parana.common.model.ParanaUser;
import io.terminus.parana.web.core.events.user.UserEvent;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class RegisterEvent extends UserEvent {
   public RegisterEvent(HttpServletRequest request, HttpServletResponse response, ParanaUser user) {
      super(request, response, user);
   }

   public RegisterEvent(HttpServletRequest request, HttpServletResponse response) {
      super(request, response);
   }
}
