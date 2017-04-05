package io.terminus.parana.settlement.model;

import java.io.Serializable;
import java.util.Date;

public class PayChannelDetail implements Serializable {
   private static final long serialVersionUID = 5125783496899866465L;
   private Long id;
   private Long refundOrderId;
   private Long fee;
   private String channel;
   private String innerChannel;
   private String systemNo;
   private String paymentCode;
   private String tradeNo;
   private String batchNo;
   private Long thirdPartyFee;
   private Integer thirdPartyRate;
   private Integer type;
   private Integer checkStatus;
   private Date paidAt;
   private Date refundAt;
   private Date checkedAt;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getRefundOrderId() {
      return this.refundOrderId;
   }

   public Long getFee() {
      return this.fee;
   }

   public String getChannel() {
      return this.channel;
   }

   public String getInnerChannel() {
      return this.innerChannel;
   }

   public String getSystemNo() {
      return this.systemNo;
   }

   public String getPaymentCode() {
      return this.paymentCode;
   }

   public String getTradeNo() {
      return this.tradeNo;
   }

   public String getBatchNo() {
      return this.batchNo;
   }

   public Long getThirdPartyFee() {
      return this.thirdPartyFee;
   }

   public Integer getThirdPartyRate() {
      return this.thirdPartyRate;
   }

   public Integer getType() {
      return this.type;
   }

   public Integer getCheckStatus() {
      return this.checkStatus;
   }

   public Date getPaidAt() {
      return this.paidAt;
   }

   public Date getRefundAt() {
      return this.refundAt;
   }

   public Date getCheckedAt() {
      return this.checkedAt;
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

   public void setRefundOrderId(Long refundOrderId) {
      this.refundOrderId = refundOrderId;
   }

   public void setFee(Long fee) {
      this.fee = fee;
   }

   public void setChannel(String channel) {
      this.channel = channel;
   }

   public void setInnerChannel(String innerChannel) {
      this.innerChannel = innerChannel;
   }

   public void setSystemNo(String systemNo) {
      this.systemNo = systemNo;
   }

   public void setPaymentCode(String paymentCode) {
      this.paymentCode = paymentCode;
   }

   public void setTradeNo(String tradeNo) {
      this.tradeNo = tradeNo;
   }

   public void setBatchNo(String batchNo) {
      this.batchNo = batchNo;
   }

   public void setThirdPartyFee(Long thirdPartyFee) {
      this.thirdPartyFee = thirdPartyFee;
   }

   public void setThirdPartyRate(Integer thirdPartyRate) {
      this.thirdPartyRate = thirdPartyRate;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public void setCheckStatus(Integer checkStatus) {
      this.checkStatus = checkStatus;
   }

   public void setPaidAt(Date paidAt) {
      this.paidAt = paidAt;
   }

   public void setRefundAt(Date refundAt) {
      this.refundAt = refundAt;
   }

   public void setCheckedAt(Date checkedAt) {
      this.checkedAt = checkedAt;
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
      } else if(!(o instanceof PayChannelDetail)) {
         return false;
      } else {
         PayChannelDetail other = (PayChannelDetail)o;
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

            Object this$refundOrderId = this.getRefundOrderId();
            Object other$refundOrderId = other.getRefundOrderId();
            if(this$refundOrderId == null) {
               if(other$refundOrderId != null) {
                  return false;
               }
            } else if(!this$refundOrderId.equals(other$refundOrderId)) {
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

            Object this$innerChannel = this.getInnerChannel();
            Object other$innerChannel = other.getInnerChannel();
            if(this$innerChannel == null) {
               if(other$innerChannel != null) {
                  return false;
               }
            } else if(!this$innerChannel.equals(other$innerChannel)) {
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

            Object this$paymentCode = this.getPaymentCode();
            Object other$paymentCode = other.getPaymentCode();
            if(this$paymentCode == null) {
               if(other$paymentCode != null) {
                  return false;
               }
            } else if(!this$paymentCode.equals(other$paymentCode)) {
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

            Object this$batchNo = this.getBatchNo();
            Object other$batchNo = other.getBatchNo();
            if(this$batchNo == null) {
               if(other$batchNo != null) {
                  return false;
               }
            } else if(!this$batchNo.equals(other$batchNo)) {
               return false;
            }

            Object this$thirdPartyFee = this.getThirdPartyFee();
            Object other$thirdPartyFee = other.getThirdPartyFee();
            if(this$thirdPartyFee == null) {
               if(other$thirdPartyFee != null) {
                  return false;
               }
            } else if(!this$thirdPartyFee.equals(other$thirdPartyFee)) {
               return false;
            }

            Object this$thirdPartyRate = this.getThirdPartyRate();
            Object other$thirdPartyRate = other.getThirdPartyRate();
            if(this$thirdPartyRate == null) {
               if(other$thirdPartyRate != null) {
                  return false;
               }
            } else if(!this$thirdPartyRate.equals(other$thirdPartyRate)) {
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

            Object this$checkStatus = this.getCheckStatus();
            Object other$checkStatus = other.getCheckStatus();
            if(this$checkStatus == null) {
               if(other$checkStatus != null) {
                  return false;
               }
            } else if(!this$checkStatus.equals(other$checkStatus)) {
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

            Object this$refundAt = this.getRefundAt();
            Object other$refundAt = other.getRefundAt();
            if(this$refundAt == null) {
               if(other$refundAt != null) {
                  return false;
               }
            } else if(!this$refundAt.equals(other$refundAt)) {
               return false;
            }

            Object this$checkedAt = this.getCheckedAt();
            Object other$checkedAt = other.getCheckedAt();
            if(this$checkedAt == null) {
               if(other$checkedAt != null) {
                  return false;
               }
            } else if(!this$checkedAt.equals(other$checkedAt)) {
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
      return other instanceof PayChannelDetail;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $refundOrderId = this.getRefundOrderId();
      result = result * 31 + ($refundOrderId == null?0:$refundOrderId.hashCode());
      Object $fee = this.getFee();
      result = result * 31 + ($fee == null?0:$fee.hashCode());
      Object $channel = this.getChannel();
      result = result * 31 + ($channel == null?0:$channel.hashCode());
      Object $innerChannel = this.getInnerChannel();
      result = result * 31 + ($innerChannel == null?0:$innerChannel.hashCode());
      Object $systemNo = this.getSystemNo();
      result = result * 31 + ($systemNo == null?0:$systemNo.hashCode());
      Object $paymentCode = this.getPaymentCode();
      result = result * 31 + ($paymentCode == null?0:$paymentCode.hashCode());
      Object $tradeNo = this.getTradeNo();
      result = result * 31 + ($tradeNo == null?0:$tradeNo.hashCode());
      Object $batchNo = this.getBatchNo();
      result = result * 31 + ($batchNo == null?0:$batchNo.hashCode());
      Object $thirdPartyFee = this.getThirdPartyFee();
      result = result * 31 + ($thirdPartyFee == null?0:$thirdPartyFee.hashCode());
      Object $thirdPartyRate = this.getThirdPartyRate();
      result = result * 31 + ($thirdPartyRate == null?0:$thirdPartyRate.hashCode());
      Object $type = this.getType();
      result = result * 31 + ($type == null?0:$type.hashCode());
      Object $checkStatus = this.getCheckStatus();
      result = result * 31 + ($checkStatus == null?0:$checkStatus.hashCode());
      Object $paidAt = this.getPaidAt();
      result = result * 31 + ($paidAt == null?0:$paidAt.hashCode());
      Object $refundAt = this.getRefundAt();
      result = result * 31 + ($refundAt == null?0:$refundAt.hashCode());
      Object $checkedAt = this.getCheckedAt();
      result = result * 31 + ($checkedAt == null?0:$checkedAt.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "PayChannelDetail(id=" + this.getId() + ", refundOrderId=" + this.getRefundOrderId() + ", fee=" + this.getFee() + ", channel=" + this.getChannel() + ", innerChannel=" + this.getInnerChannel() + ", systemNo=" + this.getSystemNo() + ", paymentCode=" + this.getPaymentCode() + ", tradeNo=" + this.getTradeNo() + ", batchNo=" + this.getBatchNo() + ", thirdPartyFee=" + this.getThirdPartyFee() + ", thirdPartyRate=" + this.getThirdPartyRate() + ", type=" + this.getType() + ", checkStatus=" + this.getCheckStatus() + ", paidAt=" + this.getPaidAt() + ", refundAt=" + this.getRefundAt() + ", checkedAt=" + this.getCheckedAt() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
