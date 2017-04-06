package cn.blmdz.wolf.user.dto;

import java.io.Serializable;

import cn.blmdz.wolf.user.model.User;
import cn.blmdz.wolf.user.model.UserProfile;

public class UserProfileDetail implements Serializable {
   private static final long serialVersionUID = -9186603523764221012L;
   private User user;
   private UserProfile profile;

   public User getUser() {
      return this.user;
   }

   public UserProfile getProfile() {
      return this.profile;
   }

   public void setUser(User user) {
      this.user = user;
   }

   public void setProfile(UserProfile profile) {
      this.profile = profile;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof UserProfileDetail)) {
         return false;
      } else {
         UserProfileDetail other = (UserProfileDetail)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$user = this.getUser();
            Object other$user = other.getUser();
            if(this$user == null) {
               if(other$user != null) {
                  return false;
               }
            } else if(!this$user.equals(other$user)) {
               return false;
            }

            Object this$profile = this.getProfile();
            Object other$profile = other.getProfile();
            if(this$profile == null) {
               if(other$profile != null) {
                  return false;
               }
            } else if(!this$profile.equals(other$profile)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof UserProfileDetail;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $user = this.getUser();
      result = result * 59 + ($user == null?0:$user.hashCode());
      Object $profile = this.getProfile();
      result = result * 59 + ($profile == null?0:$profile.hashCode());
      return result;
   }

   public String toString() {
      return "UserProfileDetail(user=" + this.getUser() + ", profile=" + this.getProfile() + ")";
   }
}
