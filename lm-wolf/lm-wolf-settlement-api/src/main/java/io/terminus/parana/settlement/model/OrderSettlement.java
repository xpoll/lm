package io.terminus.parana.settlement.model;

import java.io.Serializable;
import java.util.Date;

public class OrderSettlement implements Serializable {
   private static final long serialVersionUID = -7856757444993304990L;
   private Long id;
   private Long orderId;
   private Long sellerId;
   private String sellerName;
   private Long shopId;
   private String shopName;
   private Long buyerId;
   private String buyerName;
   private Integer type;
   private Integer payType;
   private Boolean isChecked;
   private Integer orderStatus;
   private Boolean mergePaid;
   private String systemNo;
   private Long originFee;
   private Long sellerDiscount;
   private Long shipFee;
   private Long receivable;
   private Long platformDiscount;
   private Long fee;
   private Long refundFee;
   private Long commission;
   private Long thirdPartyFee;
   private Long shipFeeDiscount;
   private Long integral;
   private Long balance;
   private Long saleTax;
   private Integer saleTaxRate;
   private Long shipFeeSaleTax;
   private String memo;
   private Date finishedAt;
   private Date checkedAt;
   private Date createdAt;
   private Date updatedAt;

   public Integer getSaleTaxRate() {
      return this.saleTaxRate == null?Integer.valueOf(0):this.saleTaxRate;
   }

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

   public Integer getType() {
      return this.type;
   }

   public Integer getPayType() {
      return this.payType;
   }

   public Boolean getIsChecked() {
      return this.isChecked;
   }

   public Integer getOrderStatus() {
      return this.orderStatus;
   }

   public Boolean getMergePaid() {
      return this.mergePaid;
   }

   public String getSystemNo() {
      return this.systemNo;
   }

   public Long getOriginFee() {
      return this.originFee;
   }

   public Long getSellerDiscount() {
      return this.sellerDiscount;
   }

   public Long getShipFee() {
      return this.shipFee;
   }

   public Long getReceivable() {
      return this.receivable;
   }

   public Long getPlatformDiscount() {
      return this.platformDiscount;
   }

   public Long getFee() {
      return this.fee;
   }

   public Long getRefundFee() {
      return this.refundFee;
   }

   public Long getCommission() {
      return this.commission;
   }

   public Long getThirdPartyFee() {
      return this.thirdPartyFee;
   }

   public Long getShipFeeDiscount() {
      return this.shipFeeDiscount;
   }

   public Long getIntegral() {
      return this.integral;
   }

   public Long getBalance() {
      return this.balance;
   }

   public Long getSaleTax() {
      return this.saleTax;
   }

   public Long getShipFeeSaleTax() {
      return this.shipFeeSaleTax;
   }

   public String getMemo() {
      return this.memo;
   }

   public Date getFinishedAt() {
      return this.finishedAt;
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

   public void setType(Integer type) {
      this.type = type;
   }

   public void setPayType(Integer payType) {
      this.payType = payType;
   }

   public void setIsChecked(Boolean isChecked) {
      this.isChecked = isChecked;
   }

   public void setOrderStatus(Integer orderStatus) {
      this.orderStatus = orderStatus;
   }

   public void setMergePaid(Boolean mergePaid) {
      this.mergePaid = mergePaid;
   }

   public void setSystemNo(String systemNo) {
      this.systemNo = systemNo;
   }

   public void setOriginFee(Long originFee) {
      this.originFee = originFee;
   }

   public void setSellerDiscount(Long sellerDiscount) {
      this.sellerDiscount = sellerDiscount;
   }

   public void setShipFee(Long shipFee) {
      this.shipFee = shipFee;
   }

   public void setReceivable(Long receivable) {
      this.receivable = receivable;
   }

   public void setPlatformDiscount(Long platformDiscount) {
      this.platformDiscount = platformDiscount;
   }

   public void setFee(Long fee) {
      this.fee = fee;
   }

   public void setRefundFee(Long refundFee) {
      this.refundFee = refundFee;
   }

   public void setCommission(Long commission) {
      this.commission = commission;
   }

   public void setThirdPartyFee(Long thirdPartyFee) {
      this.thirdPartyFee = thirdPartyFee;
   }

   public void setShipFeeDiscount(Long shipFeeDiscount) {
      this.shipFeeDiscount = shipFeeDiscount;
   }

   public void setIntegral(Long integral) {
      this.integral = integral;
   }

   public void setBalance(Long balance) {
      this.balance = balance;
   }

   public void setSaleTax(Long saleTax) {
      this.saleTax = saleTax;
   }

   public void setSaleTaxRate(Integer saleTaxRate) {
      this.saleTaxRate = saleTaxRate;
   }

   public void setShipFeeSaleTax(Long shipFeeSaleTax) {
      this.shipFeeSaleTax = shipFeeSaleTax;
   }

   public void setMemo(String memo) {
      this.memo = memo;
   }

   public void setFinishedAt(Date finishedAt) {
      this.finishedAt = finishedAt;
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
      } else if(!(o instanceof OrderSettlement)) {
         return false;
      } else {
         OrderSettlement other = (OrderSettlement)o;
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

            Object this$isChecked = this.getIsChecked();
            Object other$isChecked = other.getIsChecked();
            if(this$isChecked == null) {
               if(other$isChecked != null) {
                  return false;
               }
            } else if(!this$isChecked.equals(other$isChecked)) {
               return false;
            }

            Object this$orderStatus = this.getOrderStatus();
            Object other$orderStatus = other.getOrderStatus();
            if(this$orderStatus == null) {
               if(other$orderStatus != null) {
                  return false;
               }
            } else if(!this$orderStatus.equals(other$orderStatus)) {
               return false;
            }

            Object this$mergePaid = this.getMergePaid();
            Object other$mergePaid = other.getMergePaid();
            if(this$mergePaid == null) {
               if(other$mergePaid != null) {
                  return false;
               }
            } else if(!this$mergePaid.equals(other$mergePaid)) {
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

            Object this$originFee = this.getOriginFee();
            Object other$originFee = other.getOriginFee();
            if(this$originFee == null) {
               if(other$originFee != null) {
                  return false;
               }
            } else if(!this$originFee.equals(other$originFee)) {
               return false;
            }

            Object this$sellerDiscount = this.getSellerDiscount();
            Object other$sellerDiscount = other.getSellerDiscount();
            if(this$sellerDiscount == null) {
               if(other$sellerDiscount != null) {
                  return false;
               }
            } else if(!this$sellerDiscount.equals(other$sellerDiscount)) {
               return false;
            }

            Object this$shipFee = this.getShipFee();
            Object other$shipFee = other.getShipFee();
            if(this$shipFee == null) {
               if(other$shipFee != null) {
                  return false;
               }
            } else if(!this$shipFee.equals(other$shipFee)) {
               return false;
            }

            Object this$receivable = this.getReceivable();
            Object other$receivable = other.getReceivable();
            if(this$receivable == null) {
               if(other$receivable != null) {
                  return false;
               }
            } else if(!this$receivable.equals(other$receivable)) {
               return false;
            }

            Object this$platformDiscount = this.getPlatformDiscount();
            Object other$platformDiscount = other.getPlatformDiscount();
            if(this$platformDiscount == null) {
               if(other$platformDiscount != null) {
                  return false;
               }
            } else if(!this$platformDiscount.equals(other$platformDiscount)) {
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

            Object this$refundFee = this.getRefundFee();
            Object other$refundFee = other.getRefundFee();
            if(this$refundFee == null) {
               if(other$refundFee != null) {
                  return false;
               }
            } else if(!this$refundFee.equals(other$refundFee)) {
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

            Object this$thirdPartyFee = this.getThirdPartyFee();
            Object other$thirdPartyFee = other.getThirdPartyFee();
            if(this$thirdPartyFee == null) {
               if(other$thirdPartyFee != null) {
                  return false;
               }
            } else if(!this$thirdPartyFee.equals(other$thirdPartyFee)) {
               return false;
            }

            Object this$shipFeeDiscount = this.getShipFeeDiscount();
            Object other$shipFeeDiscount = other.getShipFeeDiscount();
            if(this$shipFeeDiscount == null) {
               if(other$shipFeeDiscount != null) {
                  return false;
               }
            } else if(!this$shipFeeDiscount.equals(other$shipFeeDiscount)) {
               return false;
            }

            Object this$integral = this.getIntegral();
            Object other$integral = other.getIntegral();
            if(this$integral == null) {
               if(other$integral != null) {
                  return false;
               }
            } else if(!this$integral.equals(other$integral)) {
               return false;
            }

            Object this$balance = this.getBalance();
            Object other$balance = other.getBalance();
            if(this$balance == null) {
               if(other$balance != null) {
                  return false;
               }
            } else if(!this$balance.equals(other$balance)) {
               return false;
            }

            Object this$saleTax = this.getSaleTax();
            Object other$saleTax = other.getSaleTax();
            if(this$saleTax == null) {
               if(other$saleTax != null) {
                  return false;
               }
            } else if(!this$saleTax.equals(other$saleTax)) {
               return false;
            }

            Object this$saleTaxRate = this.getSaleTaxRate();
            Object other$saleTaxRate = other.getSaleTaxRate();
            if(this$saleTaxRate == null) {
               if(other$saleTaxRate != null) {
                  return false;
               }
            } else if(!this$saleTaxRate.equals(other$saleTaxRate)) {
               return false;
            }

            Object this$shipFeeSaleTax = this.getShipFeeSaleTax();
            Object other$shipFeeSaleTax = other.getShipFeeSaleTax();
            if(this$shipFeeSaleTax == null) {
               if(other$shipFeeSaleTax != null) {
                  return false;
               }
            } else if(!this$shipFeeSaleTax.equals(other$shipFeeSaleTax)) {
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

            Object this$finishedAt = this.getFinishedAt();
            Object other$finishedAt = other.getFinishedAt();
            if(this$finishedAt == null) {
               if(other$finishedAt != null) {
                  return false;
               }
            } else if(!this$finishedAt.equals(other$finishedAt)) {
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
      return other instanceof OrderSettlement;
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
      Object $type = this.getType();
      result = result * 31 + ($type == null?0:$type.hashCode());
      Object $payType = this.getPayType();
      result = result * 31 + ($payType == null?0:$payType.hashCode());
      Object $isChecked = this.getIsChecked();
      result = result * 31 + ($isChecked == null?0:$isChecked.hashCode());
      Object $orderStatus = this.getOrderStatus();
      result = result * 31 + ($orderStatus == null?0:$orderStatus.hashCode());
      Object $mergePaid = this.getMergePaid();
      result = result * 31 + ($mergePaid == null?0:$mergePaid.hashCode());
      Object $systemNo = this.getSystemNo();
      result = result * 31 + ($systemNo == null?0:$systemNo.hashCode());
      Object $originFee = this.getOriginFee();
      result = result * 31 + ($originFee == null?0:$originFee.hashCode());
      Object $sellerDiscount = this.getSellerDiscount();
      result = result * 31 + ($sellerDiscount == null?0:$sellerDiscount.hashCode());
      Object $shipFee = this.getShipFee();
      result = result * 31 + ($shipFee == null?0:$shipFee.hashCode());
      Object $receivable = this.getReceivable();
      result = result * 31 + ($receivable == null?0:$receivable.hashCode());
      Object $platformDiscount = this.getPlatformDiscount();
      result = result * 31 + ($platformDiscount == null?0:$platformDiscount.hashCode());
      Object $fee = this.getFee();
      result = result * 31 + ($fee == null?0:$fee.hashCode());
      Object $refundFee = this.getRefundFee();
      result = result * 31 + ($refundFee == null?0:$refundFee.hashCode());
      Object $commission = this.getCommission();
      result = result * 31 + ($commission == null?0:$commission.hashCode());
      Object $thirdPartyFee = this.getThirdPartyFee();
      result = result * 31 + ($thirdPartyFee == null?0:$thirdPartyFee.hashCode());
      Object $shipFeeDiscount = this.getShipFeeDiscount();
      result = result * 31 + ($shipFeeDiscount == null?0:$shipFeeDiscount.hashCode());
      Object $integral = this.getIntegral();
      result = result * 31 + ($integral == null?0:$integral.hashCode());
      Object $balance = this.getBalance();
      result = result * 31 + ($balance == null?0:$balance.hashCode());
      Object $saleTax = this.getSaleTax();
      result = result * 31 + ($saleTax == null?0:$saleTax.hashCode());
      Object $saleTaxRate = this.getSaleTaxRate();
      result = result * 31 + ($saleTaxRate == null?0:$saleTaxRate.hashCode());
      Object $shipFeeSaleTax = this.getShipFeeSaleTax();
      result = result * 31 + ($shipFeeSaleTax == null?0:$shipFeeSaleTax.hashCode());
      Object $memo = this.getMemo();
      result = result * 31 + ($memo == null?0:$memo.hashCode());
      Object $finishedAt = this.getFinishedAt();
      result = result * 31 + ($finishedAt == null?0:$finishedAt.hashCode());
      Object $checkedAt = this.getCheckedAt();
      result = result * 31 + ($checkedAt == null?0:$checkedAt.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "OrderSettlement(id=" + this.getId() + ", orderId=" + this.getOrderId() + ", sellerId=" + this.getSellerId() + ", sellerName=" + this.getSellerName() + ", shopId=" + this.getShopId() + ", shopName=" + this.getShopName() + ", buyerId=" + this.getBuyerId() + ", buyerName=" + this.getBuyerName() + ", type=" + this.getType() + ", payType=" + this.getPayType() + ", isChecked=" + this.getIsChecked() + ", orderStatus=" + this.getOrderStatus() + ", mergePaid=" + this.getMergePaid() + ", systemNo=" + this.getSystemNo() + ", originFee=" + this.getOriginFee() + ", sellerDiscount=" + this.getSellerDiscount() + ", shipFee=" + this.getShipFee() + ", receivable=" + this.getReceivable() + ", platformDiscount=" + this.getPlatformDiscount() + ", fee=" + this.getFee() + ", refundFee=" + this.getRefundFee() + ", commission=" + this.getCommission() + ", thirdPartyFee=" + this.getThirdPartyFee() + ", shipFeeDiscount=" + this.getShipFeeDiscount() + ", integral=" + this.getIntegral() + ", balance=" + this.getBalance() + ", saleTax=" + this.getSaleTax() + ", saleTaxRate=" + this.getSaleTaxRate() + ", shipFeeSaleTax=" + this.getShipFeeSaleTax() + ", memo=" + this.getMemo() + ", finishedAt=" + this.getFinishedAt() + ", checkedAt=" + this.getCheckedAt() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
