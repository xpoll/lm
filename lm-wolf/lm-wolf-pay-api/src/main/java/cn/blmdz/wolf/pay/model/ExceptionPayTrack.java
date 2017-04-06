package cn.blmdz.wolf.pay.model;

import java.io.Serializable;
import java.util.Date;

public class ExceptionPayTrack implements Serializable {
   private static final long serialVersionUID = -5623420244725045864L;
   private Long id;
   private String tradeNo;
   private String paymentCode;
   private String channel;
   private Date paidAt;
   private Integer fee;
   private String description;
   private Integer status;
   private Integer type;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public String getTradeNo() {
      return this.tradeNo;
   }

   public String getPaymentCode() {
      return this.paymentCode;
   }

   public String getChannel() {
      return this.channel;
   }

   public Date getPaidAt() {
      return this.paidAt;
   }

   public Integer getFee() {
      return this.fee;
   }

   public String getDescription() {
      return this.description;
   }

   public Integer getStatus() {
      return this.status;
   }

   public Integer getType() {
      return this.type;
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

   public void setTradeNo(String tradeNo) {
      this.tradeNo = tradeNo;
   }

   public void setPaymentCode(String paymentCode) {
      this.paymentCode = paymentCode;
   }

   public void setChannel(String channel) {
      this.channel = channel;
   }

   public void setPaidAt(Date paidAt) {
      this.paidAt = paidAt;
   }

   public void setFee(Integer fee) {
      this.fee = fee;
   }

   public void setDescription(String description) {
      this.description = description;
   }

   public void setStatus(Integer status) {
      this.status = status;
   }

   public void setType(Integer type) {
      this.type = type;
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
      } else if(!(o instanceof ExceptionPayTrack)) {
         return false;
      } else {
         ExceptionPayTrack other = (ExceptionPayTrack)o;
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

            Object this$tradeNo = this.getTradeNo();
            Object other$tradeNo = other.getTradeNo();
            if(this$tradeNo == null) {
               if(other$tradeNo != null) {
                  return false;
               }
            } else if(!this$tradeNo.equals(other$tradeNo)) {
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

            Object this$channel = this.getChannel();
            Object other$channel = other.getChannel();
            if(this$channel == null) {
               if(other$channel != null) {
                  return false;
               }
            } else if(!this$channel.equals(other$channel)) {
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

            Object this$fee = this.getFee();
            Object other$fee = other.getFee();
            if(this$fee == null) {
               if(other$fee != null) {
                  return false;
               }
            } else if(!this$fee.equals(other$fee)) {
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

            Object this$status = this.getStatus();
            Object other$status = other.getStatus();
            if(this$status == null) {
               if(other$status != null) {
                  return false;
               }
            } else if(!this$status.equals(other$status)) {
               return false;
            }

            Object this$type = this.getType();
            Object other$type = other.getType();
            if(this$type == null) {
               if(other$type != null) {
                  return false;
               }
            } else if(!this$type.equals(other$type)) {
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
      return other instanceof ExceptionPayTrack;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $tradeNo = this.getTradeNo();
      result = result * 31 + ($tradeNo == null?0:$tradeNo.hashCode());
      Object $paymentCode = this.getPaymentCode();
      result = result * 31 + ($paymentCode == null?0:$paymentCode.hashCode());
      Object $channel = this.getChannel();
      result = result * 31 + ($channel == null?0:$channel.hashCode());
      Object $paidAt = this.getPaidAt();
      result = result * 31 + ($paidAt == null?0:$paidAt.hashCode());
      Object $fee = this.getFee();
      result = result * 31 + ($fee == null?0:$fee.hashCode());
      Object $description = this.getDescription();
      result = result * 31 + ($description == null?0:$description.hashCode());
      Object $status = this.getStatus();
      result = result * 31 + ($status == null?0:$status.hashCode());
      Object $type = this.getType();
      result = result * 31 + ($type == null?0:$type.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "ExceptionPayTrack(id=" + this.getId() + ", tradeNo=" + this.getTradeNo() + ", paymentCode=" + this.getPaymentCode() + ", channel=" + this.getChannel() + ", paidAt=" + this.getPaidAt() + ", fee=" + this.getFee() + ", description=" + this.getDescription() + ", status=" + this.getStatus() + ", type=" + this.getType() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
