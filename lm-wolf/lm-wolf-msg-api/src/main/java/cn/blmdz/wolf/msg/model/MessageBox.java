package cn.blmdz.wolf.msg.model;

import java.io.Serializable;
import java.util.Date;

public class MessageBox implements Serializable {
   private static final long serialVersionUID = 6490313548386738082L;
   private Long id;
   private Long userId;
   private Integer boxIndex;
   private Long notificationId;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getUserId() {
      return this.userId;
   }

   public Integer getBoxIndex() {
      return this.boxIndex;
   }

   public Long getNotificationId() {
      return this.notificationId;
   }

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public void setUserId(Long userId) {
      this.userId = userId;
   }

   public void setBoxIndex(Integer boxIndex) {
      this.boxIndex = boxIndex;
   }

   public void setNotificationId(Long notificationId) {
      this.notificationId = notificationId;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof MessageBox)) {
         return false;
      } else {
         MessageBox other = (MessageBox)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$id = this.getId();
            Object other$id = other.getId();
            if(this$id == null) {
               if(other$id != null) {
                  return false;
               }
            } else if(!this$id.equals(other$id)) {
               return false;
            }

            Object this$userId = this.getUserId();
            Object other$userId = other.getUserId();
            if(this$userId == null) {
               if(other$userId != null) {
                  return false;
               }
            } else if(!this$userId.equals(other$userId)) {
               return false;
            }

            Object this$boxIndex = this.getBoxIndex();
            Object other$boxIndex = other.getBoxIndex();
            if(this$boxIndex == null) {
               if(other$boxIndex != null) {
                  return false;
               }
            } else if(!this$boxIndex.equals(other$boxIndex)) {
               return false;
            }

            Object this$notificationId = this.getNotificationId();
            Object other$notificationId = other.getNotificationId();
            if(this$notificationId == null) {
               if(other$notificationId != null) {
                  return false;
               }
            } else if(!this$notificationId.equals(other$notificationId)) {
               return false;
            }

            Object this$createdAt = this.getCreatedAt();
            Object other$createdAt = other.getCreatedAt();
            if(this$createdAt == null) {
               if(other$createdAt != null) {
                  return false;
               }
            } else if(!this$createdAt.equals(other$createdAt)) {
               return false;
            }

            Object this$updatedAt = this.getUpdatedAt();
            Object other$updatedAt = other.getUpdatedAt();
            if(this$updatedAt == null) {
               if(other$updatedAt != null) {
                  return false;
               }
            } else if(!this$updatedAt.equals(other$updatedAt)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof MessageBox;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $userId = this.getUserId();
      result = result * 31 + ($userId == null?0:$userId.hashCode());
      Object $boxIndex = this.getBoxIndex();
      result = result * 31 + ($boxIndex == null?0:$boxIndex.hashCode());
      Object $notificationId = this.getNotificationId();
      result = result * 31 + ($notificationId == null?0:$notificationId.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "MessageBox(id=" + this.getId() + ", userId=" + this.getUserId() + ", boxIndex=" + this.getBoxIndex() + ", notificationId=" + this.getNotificationId() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
