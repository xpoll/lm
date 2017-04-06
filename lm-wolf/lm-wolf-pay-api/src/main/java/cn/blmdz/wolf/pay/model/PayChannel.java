package cn.blmdz.wolf.pay.model;

import java.io.Serializable;
import java.util.Date;

public class PayChannel implements Serializable {
   private static final long serialVersionUID = -5314404418585707864L;
   private Long id;
   private Long stageId;
   private Integer status;
   private String tradeNo;
   private String paymentCode;
   private String channel;
   private Date paidAt;
   private String batchNo;
   private Long refundOrderId;
   private Integer orderType;
   private Integer type;
   private Integer businessType;
   private Integer fee;
   private String description;
   private String extra;
   private Integer isCreatedDetail;
   private Date refundAt;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getStageId() {
      return this.stageId;
   }

   public Integer getStatus() {
      return this.status;
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

   public String getBatchNo() {
      return this.batchNo;
   }

   public Long getRefundOrderId() {
      return this.refundOrderId;
   }

   public Integer getOrderType() {
      return this.orderType;
   }

   public Integer getType() {
      return this.type;
   }

   public Integer getBusinessType() {
      return this.businessType;
   }

   public Integer getFee() {
      return this.fee;
   }

   public String getDescription() {
      return this.description;
   }

   public String getExtra() {
      return this.extra;
   }

   public Integer getIsCreatedDetail() {
      return this.isCreatedDetail;
   }

   public Date getRefundAt() {
      return this.refundAt;
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

   public void setStageId(Long stageId) {
      this.stageId = stageId;
   }

   public void setStatus(Integer status) {
      this.status = status;
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

   public void setBatchNo(String batchNo) {
      this.batchNo = batchNo;
   }

   public void setRefundOrderId(Long refundOrderId) {
      this.refundOrderId = refundOrderId;
   }

   public void setOrderType(Integer orderType) {
      this.orderType = orderType;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public void setBusinessType(Integer businessType) {
      this.businessType = businessType;
   }

   public void setFee(Integer fee) {
      this.fee = fee;
   }

   public void setDescription(String description) {
      this.description = description;
   }

   public void setExtra(String extra) {
      this.extra = extra;
   }

   public void setIsCreatedDetail(Integer isCreatedDetail) {
      this.isCreatedDetail = isCreatedDetail;
   }

   public void setRefundAt(Date refundAt) {
      this.refundAt = refundAt;
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
      } else if(!(o instanceof PayChannel)) {
         return false;
      } else {
         PayChannel other = (PayChannel)o;
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

            Object this$stageId = this.getStageId();
            Object other$stageId = other.getStageId();
            if(this$stageId == null) {
               if(other$stageId != null) {
                  return false;
               }
            } else if(!this$stageId.equals(other$stageId)) {
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

            Object this$batchNo = this.getBatchNo();
            Object other$batchNo = other.getBatchNo();
            if(this$batchNo == null) {
               if(other$batchNo != null) {
                  return false;
               }
            } else if(!this$batchNo.equals(other$batchNo)) {
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

            Object this$orderType = this.getOrderType();
            Object other$orderType = other.getOrderType();
            if(this$orderType == null) {
               if(other$orderType != null) {
                  return false;
               }
            } else if(!this$orderType.equals(other$orderType)) {
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

            Object this$businessType = this.getBusinessType();
            Object other$businessType = other.getBusinessType();
            if(this$businessType == null) {
               if(other$businessType != null) {
                  return false;
               }
            } else if(!this$businessType.equals(other$businessType)) {
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

            Object this$extra = this.getExtra();
            Object other$extra = other.getExtra();
            if(this$extra == null) {
               if(other$extra != null) {
                  return false;
               }
            } else if(!this$extra.equals(other$extra)) {
               return false;
            }

            Object this$isCreatedDetail = this.getIsCreatedDetail();
            Object other$isCreatedDetail = other.getIsCreatedDetail();
            if(this$isCreatedDetail == null) {
               if(other$isCreatedDetail != null) {
                  return false;
               }
            } else if(!this$isCreatedDetail.equals(other$isCreatedDetail)) {
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
      return other instanceof PayChannel;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $stageId = this.getStageId();
      result = result * 31 + ($stageId == null?0:$stageId.hashCode());
      Object $status = this.getStatus();
      result = result * 31 + ($status == null?0:$status.hashCode());
      Object $tradeNo = this.getTradeNo();
      result = result * 31 + ($tradeNo == null?0:$tradeNo.hashCode());
      Object $paymentCode = this.getPaymentCode();
      result = result * 31 + ($paymentCode == null?0:$paymentCode.hashCode());
      Object $channel = this.getChannel();
      result = result * 31 + ($channel == null?0:$channel.hashCode());
      Object $paidAt = this.getPaidAt();
      result = result * 31 + ($paidAt == null?0:$paidAt.hashCode());
      Object $batchNo = this.getBatchNo();
      result = result * 31 + ($batchNo == null?0:$batchNo.hashCode());
      Object $refundOrderId = this.getRefundOrderId();
      result = result * 31 + ($refundOrderId == null?0:$refundOrderId.hashCode());
      Object $orderType = this.getOrderType();
      result = result * 31 + ($orderType == null?0:$orderType.hashCode());
      Object $type = this.getType();
      result = result * 31 + ($type == null?0:$type.hashCode());
      Object $businessType = this.getBusinessType();
      result = result * 31 + ($businessType == null?0:$businessType.hashCode());
      Object $fee = this.getFee();
      result = result * 31 + ($fee == null?0:$fee.hashCode());
      Object $description = this.getDescription();
      result = result * 31 + ($description == null?0:$description.hashCode());
      Object $extra = this.getExtra();
      result = result * 31 + ($extra == null?0:$extra.hashCode());
      Object $isCreatedDetail = this.getIsCreatedDetail();
      result = result * 31 + ($isCreatedDetail == null?0:$isCreatedDetail.hashCode());
      Object $refundAt = this.getRefundAt();
      result = result * 31 + ($refundAt == null?0:$refundAt.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "PayChannel(id=" + this.getId() + ", stageId=" + this.getStageId() + ", status=" + this.getStatus() + ", tradeNo=" + this.getTradeNo() + ", paymentCode=" + this.getPaymentCode() + ", channel=" + this.getChannel() + ", paidAt=" + this.getPaidAt() + ", batchNo=" + this.getBatchNo() + ", refundOrderId=" + this.getRefundOrderId() + ", orderType=" + this.getOrderType() + ", type=" + this.getType() + ", businessType=" + this.getBusinessType() + ", fee=" + this.getFee() + ", description=" + this.getDescription() + ", extra=" + this.getExtra() + ", isCreatedDetail=" + this.getIsCreatedDetail() + ", refundAt=" + this.getRefundAt() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
