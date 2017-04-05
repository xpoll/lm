package io.terminus.parana.msg.model;

import java.io.Serializable;
import java.util.Date;

public class ReceiverGroup implements Serializable {
   private static final long serialVersionUID = -3097497128898218970L;
   private Long id;
   private Long userId;
   private String userName;
   private String email;
   private String mobile;
   private String android;
   private String ios;
   private String wp;
   private String group1;
   private String group2;
   private String group3;
   private String group4;
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

   public String getEmail() {
      return this.email;
   }

   public String getMobile() {
      return this.mobile;
   }

   public String getAndroid() {
      return this.android;
   }

   public String getIos() {
      return this.ios;
   }

   public String getWp() {
      return this.wp;
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

   public void setEmail(String email) {
      this.email = email;
   }

   public void setMobile(String mobile) {
      this.mobile = mobile;
   }

   public void setAndroid(String android) {
      this.android = android;
   }

   public void setIos(String ios) {
      this.ios = ios;
   }

   public void setWp(String wp) {
      this.wp = wp;
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

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }

   public String toString() {
      return "ReceiverGroup(id=" + this.getId() + ", userId=" + this.getUserId() + ", userName=" + this.getUserName() + ", email=" + this.getEmail() + ", mobile=" + this.getMobile() + ", android=" + this.getAndroid() + ", ios=" + this.getIos() + ", wp=" + this.getWp() + ", group1=" + this.getGroup1() + ", group2=" + this.getGroup2() + ", group3=" + this.getGroup3() + ", group4=" + this.getGroup4() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ReceiverGroup)) {
         return false;
      } else {
         ReceiverGroup other = (ReceiverGroup)o;
         if(!other.canEqual(this)) {
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

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof ReceiverGroup;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $userId = this.getUserId();
      result = result * 31 + ($userId == null?0:$userId.hashCode());
      return result;
   }
}
