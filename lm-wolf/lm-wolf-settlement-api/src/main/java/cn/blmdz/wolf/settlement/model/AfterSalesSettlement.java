package cn.blmdz.wolf.settlement.model;

import java.io.Serializable;
import java.util.Date;

public class AfterSalesSettlement implements Serializable {
   private static final long serialVersionUID = -8074158284727428830L;
   private Long id;
   private Long orderId;
   private Long sellerId;
   private String sellerName;
   private Long shopId;
   private String shopName;
   private Long buyerId;
   private String buyerName;
   private Long refundAmount;
   private Long commission;
   private String description;
   private String channel;
   private Integer orderType;
   private Boolean isChecked;
   private Date refundAt;
   private Date checkedAt;
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

   public Long getRefundAmount() {
      return this.refundAmount;
   }

   public Long getCommission() {
      return this.commission;
   }

   public String getDescription() {
      return this.description;
   }

   public String getChannel() {
      return this.channel;
   }

   public Integer getOrderType() {
      return this.orderType;
   }

   public Boolean getIsChecked() {
      return this.isChecked;
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

   public void setRefundAmount(Long refundAmount) {
      this.refundAmount = refundAmount;
   }

   public void setCommission(Long commission) {
      this.commission = commission;
   }

   public void setDescription(String description) {
      this.description = description;
   }

   public void setChannel(String channel) {
      this.channel = channel;
   }

   public void setOrderType(Integer orderType) {
      this.orderType = orderType;
   }

   public void setIsChecked(Boolean isChecked) {
      this.isChecked = isChecked;
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
      } else if(!(o instanceof AfterSalesSettlement)) {
         return false;
      } else {
         AfterSalesSettlement other = (AfterSalesSettlement)o;
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

            Object this$refundAmount = this.getRefundAmount();
            Object other$refundAmount = other.getRefundAmount();
            if(this$refundAmount == null) {
               if(other$refundAmount != null) {
                  return false;
               }
            } else if(!this$refundAmount.equals(other$refundAmount)) {
               return false;
            }

            Object this$commission = this.getCommission();
            Object other$commission = other.getCommission();
            if(this$commission == null) {
               if(other$commission != null) {
                  return false;
               }
            } else if(!this$commission.equals(other$commission)) {
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

            Object this$channel = this.getChannel();
            Object other$channel = other.getChannel();
            if(this$channel == null) {
               if(other$channel != null) {
                  return false;
               }
            } else if(!this$channel.equals(other$channel)) {
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

            Object this$isChecked = this.getIsChecked();
            Object other$isChecked = other.getIsChecked();
            if(this$isChecked == null) {
               if(other$isChecked != null) {
                  return false;
               }
            } else if(!this$isChecked.equals(other$isChecked)) {
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
      return other instanceof AfterSalesSettlement;
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
      Object $refundAmount = this.getRefundAmount();
      result = result * 31 + ($refundAmount == null?0:$refundAmount.hashCode());
      Object $commission = this.getCommission();
      result = result * 31 + ($commission == null?0:$commission.hashCode());
      Object $description = this.getDescription();
      result = result * 31 + ($description == null?0:$description.hashCode());
      Object $channel = this.getChannel();
      result = result * 31 + ($channel == null?0:$channel.hashCode());
      Object $orderType = this.getOrderType();
      result = result * 31 + ($orderType == null?0:$orderType.hashCode());
      Object $isChecked = this.getIsChecked();
      result = result * 31 + ($isChecked == null?0:$isChecked.hashCode());
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
      return "AfterSalesSettlement(id=" + this.getId() + ", orderId=" + this.getOrderId() + ", sellerId=" + this.getSellerId() + ", sellerName=" + this.getSellerName() + ", shopId=" + this.getShopId() + ", shopName=" + this.getShopName() + ", buyerId=" + this.getBuyerId() + ", buyerName=" + this.getBuyerName() + ", refundAmount=" + this.getRefundAmount() + ", commission=" + this.getCommission() + ", description=" + this.getDescription() + ", channel=" + this.getChannel() + ", orderType=" + this.getOrderType() + ", isChecked=" + this.getIsChecked() + ", refundAt=" + this.getRefundAt() + ", checkedAt=" + this.getCheckedAt() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
