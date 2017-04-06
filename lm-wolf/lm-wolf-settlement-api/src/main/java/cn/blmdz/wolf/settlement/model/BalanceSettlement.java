package cn.blmdz.wolf.settlement.model;

import java.io.Serializable;
import java.util.Date;

public class BalanceSettlement implements Serializable {
   private static final long serialVersionUID = -1483988964205805445L;
   private Long id;
   private Long orderId;
   private Long sellerId;
   private String sellerName;
   private Long shopId;
   private String shopName;
   private Long buyerId;
   private String buyerName;
   private Long businessId;
   private String businessName;
   private Integer type;
   private Integer payType;
   private Integer orderType;
   private String channel;
   private String systemNo;
   private String tradeNo;
   private String paymentCode;
   private String batchNo;
   private Long fee;
   private String memo;
   private Date tradeAt;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getOrderId() {
      return this.orderId;
   }

   public Long getSellerId() {
      return this.sellerId;
   }

   public String getSellerName() {
      return this.sellerName;
   }

   public Long getShopId() {
      return this.shopId;
   }

   public String getShopName() {
      return this.shopName;
   }

   public Long getBuyerId() {
      return this.buyerId;
   }

   public String getBuyerName() {
      return this.buyerName;
   }

   public Long getBusinessId() {
      return this.businessId;
   }

   public String getBusinessName() {
      return this.businessName;
   }

   public Integer getType() {
      return this.type;
   }

   public Integer getPayType() {
      return this.payType;
   }

   public Integer getOrderType() {
      return this.orderType;
   }

   public String getChannel() {
      return this.channel;
   }

   public String getSystemNo() {
      return this.systemNo;
   }

   public String getTradeNo() {
      return this.tradeNo;
   }

   public String getPaymentCode() {
      return this.paymentCode;
   }

   public String getBatchNo() {
      return this.batchNo;
   }

   public Long getFee() {
      return this.fee;
   }

   public String getMemo() {
      return this.memo;
   }

   public Date getTradeAt() {
      return this.tradeAt;
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

   public void setOrderId(Long orderId) {
      this.orderId = orderId;
   }

   public void setSellerId(Long sellerId) {
      this.sellerId = sellerId;
   }

   public void setSellerName(String sellerName) {
      this.sellerName = sellerName;
   }

   public void setShopId(Long shopId) {
      this.shopId = shopId;
   }

   public void setShopName(String shopName) {
      this.shopName = shopName;
   }

   public void setBuyerId(Long buyerId) {
      this.buyerId = buyerId;
   }

   public void setBuyerName(String buyerName) {
      this.buyerName = buyerName;
   }

   public void setBusinessId(Long businessId) {
      this.businessId = businessId;
   }

   public void setBusinessName(String businessName) {
      this.businessName = businessName;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public void setPayType(Integer payType) {
      this.payType = payType;
   }

   public void setOrderType(Integer orderType) {
      this.orderType = orderType;
   }

   public void setChannel(String channel) {
      this.channel = channel;
   }

   public void setSystemNo(String systemNo) {
      this.systemNo = systemNo;
   }

   public void setTradeNo(String tradeNo) {
      this.tradeNo = tradeNo;
   }

   public void setPaymentCode(String paymentCode) {
      this.paymentCode = paymentCode;
   }

   public void setBatchNo(String batchNo) {
      this.batchNo = batchNo;
   }

   public void setFee(Long fee) {
      this.fee = fee;
   }

   public void setMemo(String memo) {
      this.memo = memo;
   }

   public void setTradeAt(Date tradeAt) {
      this.tradeAt = tradeAt;
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
      } else if(!(o instanceof BalanceSettlement)) {
         return false;
      } else {
         BalanceSettlement other = (BalanceSettlement)o;
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

            Object this$orderId = this.getOrderId();
            Object other$orderId = other.getOrderId();
            if(this$orderId == null) {
               if(other$orderId != null) {
                  return false;
               }
            } else if(!this$orderId.equals(other$orderId)) {
               return false;
            }

            Object this$sellerId = this.getSellerId();
            Object other$sellerId = other.getSellerId();
            if(this$sellerId == null) {
               if(other$sellerId != null) {
                  return false;
               }
            } else if(!this$sellerId.equals(other$sellerId)) {
               return false;
            }

            Object this$sellerName = this.getSellerName();
            Object other$sellerName = other.getSellerName();
            if(this$sellerName == null) {
               if(other$sellerName != null) {
                  return false;
               }
            } else if(!this$sellerName.equals(other$sellerName)) {
               return false;
            }

            Object this$shopId = this.getShopId();
            Object other$shopId = other.getShopId();
            if(this$shopId == null) {
               if(other$shopId != null) {
                  return false;
               }
            } else if(!this$shopId.equals(other$shopId)) {
               return false;
            }

            Object this$shopName = this.getShopName();
            Object other$shopName = other.getShopName();
            if(this$shopName == null) {
               if(other$shopName != null) {
                  return false;
               }
            } else if(!this$shopName.equals(other$shopName)) {
               return false;
            }

            Object this$buyerId = this.getBuyerId();
            Object other$buyerId = other.getBuyerId();
            if(this$buyerId == null) {
               if(other$buyerId != null) {
                  return false;
               }
            } else if(!this$buyerId.equals(other$buyerId)) {
               return false;
            }

            Object this$buyerName = this.getBuyerName();
            Object other$buyerName = other.getBuyerName();
            if(this$buyerName == null) {
               if(other$buyerName != null) {
                  return false;
               }
            } else if(!this$buyerName.equals(other$buyerName)) {
               return false;
            }

            Object this$businessId = this.getBusinessId();
            Object other$businessId = other.getBusinessId();
            if(this$businessId == null) {
               if(other$businessId != null) {
                  return false;
               }
            } else if(!this$businessId.equals(other$businessId)) {
               return false;
            }

            Object this$businessName = this.getBusinessName();
            Object other$businessName = other.getBusinessName();
            if(this$businessName == null) {
               if(other$businessName != null) {
                  return false;
               }
            } else if(!this$businessName.equals(other$businessName)) {
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

            Object this$payType = this.getPayType();
            Object other$payType = other.getPayType();
            if(this$payType == null) {
               if(other$payType != null) {
                  return false;
               }
            } else if(!this$payType.equals(other$payType)) {
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

            Object this$channel = this.getChannel();
            Object other$channel = other.getChannel();
            if(this$channel == null) {
               if(other$channel != null) {
                  return false;
               }
            } else if(!this$channel.equals(other$channel)) {
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

            Object this$batchNo = this.getBatchNo();
            Object other$batchNo = other.getBatchNo();
            if(this$batchNo == null) {
               if(other$batchNo != null) {
                  return false;
               }
            } else if(!this$batchNo.equals(other$batchNo)) {
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

            Object this$memo = this.getMemo();
            Object other$memo = other.getMemo();
            if(this$memo == null) {
               if(other$memo != null) {
                  return false;
               }
            } else if(!this$memo.equals(other$memo)) {
               return false;
            }

            Object this$tradeAt = this.getTradeAt();
            Object other$tradeAt = other.getTradeAt();
            if(this$tradeAt == null) {
               if(other$tradeAt != null) {
                  return false;
               }
            } else if(!this$tradeAt.equals(other$tradeAt)) {
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
      return other instanceof BalanceSettlement;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $orderId = this.getOrderId();
      result = result * 31 + ($orderId == null?0:$orderId.hashCode());
      Object $sellerId = this.getSellerId();
      result = result * 31 + ($sellerId == null?0:$sellerId.hashCode());
      Object $sellerName = this.getSellerName();
      result = result * 31 + ($sellerName == null?0:$sellerName.hashCode());
      Object $shopId = this.getShopId();
      result = result * 31 + ($shopId == null?0:$shopId.hashCode());
      Object $shopName = this.getShopName();
      result = result * 31 + ($shopName == null?0:$shopName.hashCode());
      Object $buyerId = this.getBuyerId();
      result = result * 31 + ($buyerId == null?0:$buyerId.hashCode());
      Object $buyerName = this.getBuyerName();
      result = result * 31 + ($buyerName == null?0:$buyerName.hashCode());
      Object $businessId = this.getBusinessId();
      result = result * 31 + ($businessId == null?0:$businessId.hashCode());
      Object $businessName = this.getBusinessName();
      result = result * 31 + ($businessName == null?0:$businessName.hashCode());
      Object $type = this.getType();
      result = result * 31 + ($type == null?0:$type.hashCode());
      Object $payType = this.getPayType();
      result = result * 31 + ($payType == null?0:$payType.hashCode());
      Object $orderType = this.getOrderType();
      result = result * 31 + ($orderType == null?0:$orderType.hashCode());
      Object $channel = this.getChannel();
      result = result * 31 + ($channel == null?0:$channel.hashCode());
      Object $systemNo = this.getSystemNo();
      result = result * 31 + ($systemNo == null?0:$systemNo.hashCode());
      Object $tradeNo = this.getTradeNo();
      result = result * 31 + ($tradeNo == null?0:$tradeNo.hashCode());
      Object $paymentCode = this.getPaymentCode();
      result = result * 31 + ($paymentCode == null?0:$paymentCode.hashCode());
      Object $batchNo = this.getBatchNo();
      result = result * 31 + ($batchNo == null?0:$batchNo.hashCode());
      Object $fee = this.getFee();
      result = result * 31 + ($fee == null?0:$fee.hashCode());
      Object $memo = this.getMemo();
      result = result * 31 + ($memo == null?0:$memo.hashCode());
      Object $tradeAt = this.getTradeAt();
      result = result * 31 + ($tradeAt == null?0:$tradeAt.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "BalanceSettlement(id=" + this.getId() + ", orderId=" + this.getOrderId() + ", sellerId=" + this.getSellerId() + ", sellerName=" + this.getSellerName() + ", shopId=" + this.getShopId() + ", shopName=" + this.getShopName() + ", buyerId=" + this.getBuyerId() + ", buyerName=" + this.getBuyerName() + ", businessId=" + this.getBusinessId() + ", businessName=" + this.getBusinessName() + ", type=" + this.getType() + ", payType=" + this.getPayType() + ", orderType=" + this.getOrderType() + ", channel=" + this.getChannel() + ", systemNo=" + this.getSystemNo() + ", tradeNo=" + this.getTradeNo() + ", paymentCode=" + this.getPaymentCode() + ", batchNo=" + this.getBatchNo() + ", fee=" + this.getFee() + ", memo=" + this.getMemo() + ", tradeAt=" + this.getTradeAt() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
