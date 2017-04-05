package io.terminus.parana.web.core.events.user;

import io.terminus.parana.common.model.ParanaUser;
import java.io.Serializable;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public abstract class UserEvent implements Serializable {
   private static final long serialVersionUID = 7614783431931285762L;
   private HttpServletRequest request;
   private HttpServletResponse response;
   private final ParanaUser user;

   public UserEvent(HttpServletRequest request, HttpServletResponse response) {
      this(request, response, (ParanaUser)null);
   }

   public UserEvent(HttpServletRequest request, HttpServletResponse response, ParanaUser user) {
      this.request = request;
      this.response = response;
      this.user = user;
   }

   public HttpServletRequest getRequest() {
      return this.request;
   }

   public HttpServletResponse getResponse() {
      return this.response;
   }

   public ParanaUser getUser() {
      return this.user;
   }
}
