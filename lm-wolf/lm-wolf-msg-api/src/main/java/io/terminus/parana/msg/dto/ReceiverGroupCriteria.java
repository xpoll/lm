package io.terminus.parana.msg.dto;

import io.terminus.parana.msg.dto.PagingCriteria;

public class ReceiverGroupCriteria extends PagingCriteria {
   private static final long serialVersionUID = -1534995084257513653L;
   private String userId;
   private String userName;
   private String group1;
   private String group2;
   private String group3;
   private String group4;

   public String getUserId() {
      return this.userId;
   }

   public String getUserName() {
      return this.userName;
   }

   public String getGroup1() {
      return this.group1;
   }

   public String getGroup2() {
      return this.group2;
   }

   public String getGroup3() {
      return this.group3;
   }

   public String getGroup4() {
      return this.group4;
   }

   public void setUserId(String userId) {
      this.userId = userId;
   }

   public void setUserName(String userName) {
      this.userName = userName;
   }

   public void setGroup1(String group1) {
      this.group1 = group1;
   }

   public void setGroup2(String group2) {
      this.group2 = group2;
   }

   public void setGroup3(String group3) {
      this.group3 = group3;
   }

   public void setGroup4(String group4) {
      this.group4 = group4;
   }

   public String toString() {
      return "ReceiverGroupCriteria(userId=" + this.getUserId() + ", userName=" + this.getUserName() + ", group1=" + this.getGroup1() + ", group2=" + this.getGroup2() + ", group3=" + this.getGroup3() + ", group4=" + this.getGroup4() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ReceiverGroupCriteria)) {
         return false;
      } else {
         ReceiverGroupCriteria other = (ReceiverGroupCriteria)o;
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

            Object this$userName = this.getUserName();
            Object other$userName = other.getUserName();
            if(this$userName == null) {
               if(other$userName != null) {
                  return false;
               }
            } else if(!this$userName.equals(other$userName)) {
               return false;
            }

            Object this$group1 = this.getGroup1();
            Object other$group1 = other.getGroup1();
            if(this$group1 == null) {
               if(other$group1 != null) {
                  return false;
               }
            } else if(!this$group1.equals(other$group1)) {
               return false;
            }

            Object this$group2 = this.getGroup2();
            Object other$group2 = other.getGroup2();
            if(this$group2 == null) {
               if(other$group2 != null) {
                  return false;
               }
            } else if(!this$group2.equals(other$group2)) {
               return false;
            }

            Object this$group3 = this.getGroup3();
            Object other$group3 = other.getGroup3();
            if(this$group3 == null) {
               if(other$group3 != null) {
                  return false;
               }
            } else if(!this$group3.equals(other$group3)) {
               return false;
            }

            Object this$group4 = this.getGroup4();
            Object other$group4 = other.getGroup4();
            if(this$group4 == null) {
               if(other$group4 != null) {
                  return false;
               }
            } else if(!this$group4.equals(other$group4)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof ReceiverGroupCriteria;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      result = result * 31 + super.hashCode();
      Object $userId = this.getUserId();
      result = result * 31 + ($userId == null?0:$userId.hashCode());
      Object $userName = this.getUserName();
      result = result * 31 + ($userName == null?0:$userName.hashCode());
      Object $group1 = this.getGroup1();
      result = result * 31 + ($group1 == null?0:$group1.hashCode());
      Object $group2 = this.getGroup2();
      result = result * 31 + ($group2 == null?0:$group2.hashCode());
      Object $group3 = this.getGroup3();
      result = result * 31 + ($group3 == null?0:$group3.hashCode());
      Object $group4 = this.getGroup4();
      result = result * 31 + ($group4 == null?0:$group4.hashCode());
      return result;
   }
}
