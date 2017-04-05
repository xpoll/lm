package io.terminus.parana.msg.model;

import java.io.Serializable;
import java.util.Date;

public class Subscription implements Serializable {
   private static final long serialVersionUID = -8459033656787220327L;
   private Long id;
   private Integer channel;
   private Long userId;
   private String userName;
   private String account;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

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

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setId(Long id) {
      this.id = id;
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

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }

   public String toString() {
      return "Subscription(id=" + this.getId() + ", channel=" + this.getChannel() + ", userId=" + this.getUserId() + ", userName=" + this.getUserName() + ", account=" + this.getAccount() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Subscription)) {
         return false;
      } else {
         Subscription other = (Subscription)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
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
      return other instanceof Subscription;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $account = this.getAccount();
      result = result * 31 + ($account == null?0:$account.hashCode());
      return result;
   }
}
