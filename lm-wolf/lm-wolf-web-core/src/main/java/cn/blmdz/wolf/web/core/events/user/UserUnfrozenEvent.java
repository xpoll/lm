package cn.blmdz.wolf.web.core.events.user;

import java.io.Serializable;

public class UserUnfrozenEvent implements Serializable {
   private static final long serialVersionUID = 8800453857299602788L;
   private final Long userId;
   private final Long operatorId;

   public UserUnfrozenEvent(Long userId, Long operatorId) {
      this.userId = userId;
      this.operatorId = operatorId;
   }

   public Long getUserId() {
      return this.userId;
   }

   public Long getOperatorId() {
      return this.operatorId;
   }
}
