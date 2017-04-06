package cn.blmdz.wolf.pay.dto;

import java.io.Serializable;

public class TransferInfo implements Serializable {
   private static final long serialVersionUID = 2961247782396407409L;
   private Integer amount;
   private String tradeNo;
   private String account;
   private String description;
   private Long sumId;

   public Integer getAmount() {
      return this.amount;
   }

   public String getTradeNo() {
      return this.tradeNo;
   }

   public String getAccount() {
      return this.account;
   }

   public String getDescription() {
      return this.description;
   }

   public Long getSumId() {
      return this.sumId;
   }

   public void setAmount(Integer amount) {
      this.amount = amount;
   }

   public void setTradeNo(String tradeNo) {
      this.tradeNo = tradeNo;
   }

   public void setAccount(String account) {
      this.account = account;
   }

   public void setDescription(String description) {
      this.description = description;
   }

   public void setSumId(Long sumId) {
      this.sumId = sumId;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof TransferInfo)) {
         return false;
      } else {
         TransferInfo other = (TransferInfo)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$amount = this.getAmount();
            Object other$amount = other.getAmount();
            if(this$amount == null) {
               if(other$amount != null) {
                  return false;
               }
            } else if(!this$amount.equals(other$amount)) {
               return false;
            }

            Object this$tradeNo = this.getTradeNo();
            Object other$tradeNo = other.getTradeNo();
            if(this$tradeNo == null) {
               if(other$tradeNo != null) {
                  return false;
               }
            } else if(!this$tradeNo.equals(other$tradeNo)) {
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

            Object this$description = this.getDescription();
            Object other$description = other.getDescription();
            if(this$description == null) {
               if(other$description != null) {
                  return false;
               }
            } else if(!this$description.equals(other$description)) {
               return false;
            }

            Object this$sumId = this.getSumId();
            Object other$sumId = other.getSumId();
            if(this$sumId == null) {
               if(other$sumId != null) {
                  return false;
               }
            } else if(!this$sumId.equals(other$sumId)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof TransferInfo;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $amount = this.getAmount();
      result = result * 31 + ($amount == null?0:$amount.hashCode());
      Object $tradeNo = this.getTradeNo();
      result = result * 31 + ($tradeNo == null?0:$tradeNo.hashCode());
      Object $account = this.getAccount();
      result = result * 31 + ($account == null?0:$account.hashCode());
      Object $description = this.getDescription();
      result = result * 31 + ($description == null?0:$description.hashCode());
      Object $sumId = this.getSumId();
      result = result * 31 + ($sumId == null?0:$sumId.hashCode());
      return result;
   }

   public String toString() {
      return "TransferInfo(amount=" + this.getAmount() + ", tradeNo=" + this.getTradeNo() + ", account=" + this.getAccount() + ", description=" + this.getDescription() + ", sumId=" + this.getSumId() + ")";
   }
}
