package io.terminus.parana.msg.dto;

import io.terminus.parana.msg.dto.PagingCriteria;

public class SubscriptionCriteria extends PagingCriteria {
   private static final long serialVersionUID = 8834315055238457414L;
   private Integer channel;
   private Long userId;
   private String userName;
   private String account;

   public Integer getChannel() {
      return this.channel;
   }

   public Long getUserId() {
      return this.userId;
   }

   public String getUserName() {
      return this.userName;
   }

   public String getAccount() {
      return this.account;
   }

   public void setChannel(Integer channel) {
      this.channel = channel;
   }

   public void setUserId(Long userId) {
      this.userId = userId;
   }

   public void setUserName(String userName) {
      this.userName = userName;
   }

   public void setAccount(String account) {
      this.account = account;
   }

   public String toString() {
      return "SubscriptionCriteria(channel=" + this.getChannel() + ", userId=" + this.getUserId() + ", userName=" + this.getUserName() + ", account=" + this.getAccount() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof SubscriptionCriteria)) {
         return false;
      } else {
         SubscriptionCriteria other = (SubscriptionCriteria)o;
         if(!other.canEqual(this)) {
            return false;
         } else if(!super.equals(o)) {
            return false;
         } else {
            Object this$channel = this.getChannel();
            Object other$channel = other.getChannel();
            if(this$channel == null) {
               if(other$channel != null) {
                  return false;
               }
            } else if(!this$channel.equals(other$channel)) {
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

            Object this$account = this.getAccount();
            Object other$account = other.getAccount();
            if(this$account == null) {
               if(other$account != null) {
                  return false;
               }
            } else if(!this$account.equals(other$account)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof SubscriptionCriteria;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      result = result * 31 + super.hashCode();
      Object $channel = this.getChannel();
      result = result * 31 + ($channel == null?0:$channel.hashCode());
      Object $userId = this.getUserId();
      result = result * 31 + ($userId == null?0:$userId.hashCode());
      Object $userName = this.getUserName();
      result = result * 31 + ($userName == null?0:$userName.hashCode());
      Object $account = this.getAccount();
      result = result * 31 + ($account == null?0:$account.hashCode());
      return result;
   }
}
