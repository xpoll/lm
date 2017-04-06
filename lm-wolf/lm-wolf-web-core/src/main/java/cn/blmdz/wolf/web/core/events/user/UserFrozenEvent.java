package cn.blmdz.wolf.web.core.events.user;

import java.io.Serializable;

public class UserFrozenEvent implements Serializable {
   private static final long serialVersionUID = 1005275023594833360L;
   private final Long userId;
   private final Long operatorId;

   public UserFrozenEvent(Long userId, Long operatorId) {
      this.userId = userId;
      this.operatorId = operatorId;
   }

   public String toString() {
      return "UserFrozenEvent(userId=" + this.getUserId() + ", operatorId=" + this.getOperatorId() + ")";
   }

   public Long getUserId() {
      return this.userId;
   }

   public Long getOperatorId() {
      return this.operatorId;
   }
}
