package io.terminus.parana.msg.dto;

import io.terminus.parana.msg.dto.PagingCriteria;

public class NotificationCriteria extends PagingCriteria {
   private static final long serialVersionUID = 869228332147811753L;
   private Long userId;
   private Boolean showChecked;
   private Integer groupMessageType = Integer.valueOf(0);

   public Long getUserId() {
      return this.userId;
   }

   public Boolean getShowChecked() {
      return this.showChecked;
   }

   public Integer getGroupMessageType() {
      return this.groupMessageType;
   }

   public void setUserId(Long userId) {
      this.userId = userId;
   }

   public void setShowChecked(Boolean showChecked) {
      this.showChecked = showChecked;
   }

   public void setGroupMessageType(Integer groupMessageType) {
      this.groupMessageType = groupMessageType;
   }

   public String toString() {
      return "NotificationCriteria(userId=" + this.getUserId() + ", showChecked=" + this.getShowChecked() + ", groupMessageType=" + this.getGroupMessageType() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof NotificationCriteria)) {
         return false;
      } else {
         NotificationCriteria other = (NotificationCriteria)o;
         if(!other.canEqual(this)) {
            return false;
         } else if(!super.equals(o)) {
            return false;
         } else {
            Object this$userId = this.getUserId();
            Object other$userId = other.getUserId();
            if(this$userId == null) {
               if(other$userId != null) {
                  return false;
               }
            } else if(!this$userId.equals(other$userId)) {
               return false;
            }

            Object this$showChecked = this.getShowChecked();
            Object other$showChecked = other.getShowChecked();
            if(this$showChecked == null) {
               if(other$showChecked != null) {
                  return false;
               }
            } else if(!this$showChecked.equals(other$showChecked)) {
               return false;
            }

            Object this$groupMessageType = this.getGroupMessageType();
            Object other$groupMessageType = other.getGroupMessageType();
            if(this$groupMessageType == null) {
               if(other$groupMessageType != null) {
                  return false;
               }
            } else if(!this$groupMessageType.equals(other$groupMessageType)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof NotificationCriteria;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      result = result * 31 + super.hashCode();
      Object $userId = this.getUserId();
      result = result * 31 + ($userId == null?0:$userId.hashCode());
      Object $showChecked = this.getShowChecked();
      result = result * 31 + ($showChecked == null?0:$showChecked.hashCode());
      Object $groupMessageType = this.getGroupMessageType();
      result = result * 31 + ($groupMessageType == null?0:$groupMessageType.hashCode());
      return result;
   }
}
