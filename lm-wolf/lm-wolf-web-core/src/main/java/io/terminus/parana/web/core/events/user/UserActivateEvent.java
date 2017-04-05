package io.terminus.parana.web.core.events.user;

import java.io.Serializable;

public class UserActivateEvent implements Serializable {
   private static final long serialVersionUID = 7806570333928419259L;
   private final Long userId;

   public UserActivateEvent(Long userId) {
      this.userId = userId;
   }

   public Long getUserId() {
      return this.userId;
   }
}
