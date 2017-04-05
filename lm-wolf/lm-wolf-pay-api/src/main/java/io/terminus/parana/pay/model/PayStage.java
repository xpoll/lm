package io.terminus.parana.pay.model;

import java.io.Serializable;
import java.util.Date;

public class PayStage implements Serializable {
   private static final long serialVersionUID = -7045090173067244272L;
   private Long id;
   private Long payId;
   private Integer currentStage;
   private String content;
   private Integer fee;
   private String channel;
   private String paymentCode;
   private Integer paidStatus;
   private String systemNo;
   private Date expiredAt;
   private Date repayAt;
   private Date paidAt;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getPayId() {
      return this.payId;
   }

   public Integer getCurrentStage() {
      return this.currentStage;
   }

   public String getContent() {
      return this.content;
   }

   public Integer getFee() {
      return this.fee;
   }

   public String getChannel() {
      return this.channel;
   }

   public String getPaymentCode() {
      return this.paymentCode;
   }

   public Integer getPaidStatus() {
      return this.paidStatus;
   }

   public String getSystemNo() {
      return this.systemNo;
   }

   public Date getExpiredAt() {
      return this.expiredAt;
   }

   public Date getRepayAt() {
      return this.repayAt;
   }

   public Date getPaidAt() {
      return this.paidAt;
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

   public void setPayId(Long payId) {
      this.payId = payId;
   }

   public void setCurrentStage(Integer currentStage) {
      this.currentStage = currentStage;
   }

   public void setContent(String content) {
      this.content = content;
   }

   public void setFee(Integer fee) {
      this.fee = fee;
   }

   public void setChannel(String channel) {
      this.channel = channel;
   }

   public void setPaymentCode(String paymentCode) {
      this.paymentCode = paymentCode;
   }

   public void setPaidStatus(Integer paidStatus) {
      this.paidStatus = paidStatus;
   }

   public void setSystemNo(String systemNo) {
      this.systemNo = systemNo;
   }

   public void setExpiredAt(Date expiredAt) {
      this.expiredAt = expiredAt;
   }

   public void setRepayAt(Date repayAt) {
      this.repayAt = repayAt;
   }

   public void setPaidAt(Date paidAt) {
      this.paidAt = paidAt;
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
      } else if(!(o instanceof PayStage)) {
         return false;
      } else {
         PayStage other = (PayStage)o;
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

            Object this$payId = this.getPayId();
            Object other$payId = other.getPayId();
            if(this$payId == null) {
               if(other$payId != null) {
                  return false;
               }
            } else if(!this$payId.equals(other$payId)) {
               return false;
            }

            Object this$currentStage = this.getCurrentStage();
            Object other$currentStage = other.getCurrentStage();
            if(this$currentStage == null) {
               if(other$currentStage != null) {
                  return false;
               }
            } else if(!this$currentStage.equals(other$currentStage)) {
               return false;
            }

            Object this$content = this.getContent();
            Object other$content = other.getContent();
            if(this$content == null) {
               if(other$content != null) {
                  return false;
               }
            } else if(!this$content.equals(other$content)) {
               return false;
            }

            Object this$fee = this.getFee();
            Object other$fee = other.getFee();
            if(this$fee == null) {
               if(other$fee != null) {
                  return false;
               }
            } else if(!this$fee.equals(other$fee)) {
               return false;
            }

            Object this$channel = this.getChannel();
            Object other$channel = other.getChannel();
            if(this$channel == null) {
               if(other$channel != null) {
                  return false;
               }
            } else if(!this$channel.equals(other$channel)) {
               return false;
            }

            Object this$paymentCode = this.getPaymentCode();
            Object other$paymentCode = other.getPaymentCode();
            if(this$paymentCode == null) {
               if(other$paymentCode != null) {
                  return false;
               }
            } else if(!this$paymentCode.equals(other$paymentCode)) {
               return false;
            }

            Object this$paidStatus = this.getPaidStatus();
            Object other$paidStatus = other.getPaidStatus();
            if(this$paidStatus == null) {
               if(other$paidStatus != null) {
                  return false;
               }
            } else if(!this$paidStatus.equals(other$paidStatus)) {
               return false;
            }

            Object this$systemNo = this.getSystemNo();
            Object other$systemNo = other.getSystemNo();
            if(this$systemNo == null) {
               if(other$systemNo != null) {
                  return false;
               }
            } else if(!this$systemNo.equals(other$systemNo)) {
               return false;
            }

            Object this$expiredAt = this.getExpiredAt();
            Object other$expiredAt = other.getExpiredAt();
            if(this$expiredAt == null) {
               if(other$expiredAt != null) {
                  return false;
               }
            } else if(!this$expiredAt.equals(other$expiredAt)) {
               return false;
            }

            Object this$repayAt = this.getRepayAt();
            Object other$repayAt = other.getRepayAt();
            if(this$repayAt == null) {
               if(other$repayAt != null) {
                  return false;
               }
            } else if(!this$repayAt.equals(other$repayAt)) {
               return false;
            }

            Object this$paidAt = this.getPaidAt();
            Object other$paidAt = other.getPaidAt();
            if(this$paidAt == null) {
               if(other$paidAt != null) {
                  return false;
               }
            } else if(!this$paidAt.equals(other$paidAt)) {
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
      return other instanceof PayStage;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $payId = this.getPayId();
      result = result * 31 + ($payId == null?0:$payId.hashCode());
      Object $currentStage = this.getCurrentStage();
      result = result * 31 + ($currentStage == null?0:$currentStage.hashCode());
      Object $content = this.getContent();
      result = result * 31 + ($content == null?0:$content.hashCode());
      Object $fee = this.getFee();
      result = result * 31 + ($fee == null?0:$fee.hashCode());
      Object $channel = this.getChannel();
      result = result * 31 + ($channel == null?0:$channel.hashCode());
      Object $paymentCode = this.getPaymentCode();
      result = result * 31 + ($paymentCode == null?0:$paymentCode.hashCode());
      Object $paidStatus = this.getPaidStatus();
      result = result * 31 + ($paidStatus == null?0:$paidStatus.hashCode());
      Object $systemNo = this.getSystemNo();
      result = result * 31 + ($systemNo == null?0:$systemNo.hashCode());
      Object $expiredAt = this.getExpiredAt();
      result = result * 31 + ($expiredAt == null?0:$expiredAt.hashCode());
      Object $repayAt = this.getRepayAt();
      result = result * 31 + ($repayAt == null?0:$repayAt.hashCode());
      Object $paidAt = this.getPaidAt();
      result = result * 31 + ($paidAt == null?0:$paidAt.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "PayStage(id=" + this.getId() + ", payId=" + this.getPayId() + ", currentStage=" + this.getCurrentStage() + ", content=" + this.getContent() + ", fee=" + this.getFee() + ", channel=" + this.getChannel() + ", paymentCode=" + this.getPaymentCode() + ", paidStatus=" + this.getPaidStatus() + ", systemNo=" + this.getSystemNo() + ", expiredAt=" + this.getExpiredAt() + ", repayAt=" + this.getRepayAt() + ", paidAt=" + this.getPaidAt() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
