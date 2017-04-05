package io.terminus.parana.user.model;

import java.io.Serializable;
import java.util.Date;

public class UserDevice implements Serializable {
   private static final long serialVersionUID = 0L;
   private Long id;
   private Long userId;
   private String userName;
   private String deviceToken;
   private String deviceType;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getUserId() {
      return this.userId;
   }

   public String getUserName() {
      return this.userName;
   }

   public String getDeviceToken() {
      return this.deviceToken;
   }

   public String getDeviceType() {
      return this.deviceType;
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

   public void setUserName(String userName) {
      this.userName = userName;
   }

   public void setDeviceToken(String deviceToken) {
      this.deviceToken = deviceToken;
   }

   public void setDeviceType(String deviceType) {
      this.deviceType = deviceType;
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
      } else if(!(o instanceof UserDevice)) {
         return false;
      } else {
         UserDevice other = (UserDevice)o;
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

            Object this$userName = this.getUserName();
            Object other$userName = other.getUserName();
            if(this$userName == null) {
               if(other$userName != null) {
                  return false;
               }
            } else if(!this$userName.equals(other$userName)) {
               return false;
            }

            Object this$deviceToken = this.getDeviceToken();
            Object other$deviceToken = other.getDeviceToken();
            if(this$deviceToken == null) {
               if(other$deviceToken != null) {
                  return false;
               }
            } else if(!this$deviceToken.equals(other$deviceToken)) {
               return false;
            }

            Object this$deviceType = this.getDeviceType();
            Object other$deviceType = other.getDeviceType();
            if(this$deviceType == null) {
               if(other$deviceType != null) {
                  return false;
               }
            } else if(!this$deviceType.equals(other$deviceType)) {
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
      return other instanceof UserDevice;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $userId = this.getUserId();
      result = result * 31 + ($userId == null?0:$userId.hashCode());
      Object $userName = this.getUserName();
      result = result * 31 + ($userName == null?0:$userName.hashCode());
      Object $deviceToken = this.getDeviceToken();
      result = result * 31 + ($deviceToken == null?0:$deviceToken.hashCode());
      Object $deviceType = this.getDeviceType();
      result = result * 31 + ($deviceType == null?0:$deviceType.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "UserDevice(id=" + this.getId() + ", userId=" + this.getUserId() + ", userName=" + this.getUserName() + ", deviceToken=" + this.getDeviceToken() + ", deviceType=" + this.getDeviceType() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
