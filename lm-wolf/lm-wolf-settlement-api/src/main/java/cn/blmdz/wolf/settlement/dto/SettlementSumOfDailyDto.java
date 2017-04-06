package cn.blmdz.wolf.settlement.dto;

import java.io.Serializable;
import java.util.Date;

public class SettlementSumOfDailyDto implements Serializable {
   private static final long serialVersionUID = -7938526912182348061L;
   private Long sellerId;
   private String sellerName;
   private Long shopId;
   private String shopName;
   private Integer orderCount;
   private Long fee;
   private Long originFee;
   private Long sellerDiscount;
   private Long shipFee;
   private Long receivable;
   private Long platformDiscount;
   private Long refundFee;
   private Long commission;
   private Long integral;
   private Long thirdPartyFee;
   private Long saleTax;
   private Date sumAt;

   public String toString() {
      return "SettlementSumOfDailyDto(sellerId=" + this.getSellerId() + ", sellerName=" + this.getSellerName() + ", shopId=" + this.getShopId() + ", shopName=" + this.getShopName() + ", orderCount=" + this.getOrderCount() + ", fee=" + this.getFee() + ", originFee=" + this.getOriginFee() + ", sellerDiscount=" + this.getSellerDiscount() + ", shipFee=" + this.getShipFee() + ", receivable=" + this.getReceivable() + ", platformDiscount=" + this.getPlatformDiscount() + ", refundFee=" + this.getRefundFee() + ", commission=" + this.getCommission() + ", integral=" + this.getIntegral() + ", thirdPartyFee=" + this.getThirdPartyFee() + ", saleTax=" + this.getSaleTax() + ", sumAt=" + this.getSumAt() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof SettlementSumOfDailyDto)) {
         return false;
      } else {
         SettlementSumOfDailyDto other = (SettlementSumOfDailyDto)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
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

            Object this$orderCount = this.getOrderCount();
            Object other$orderCount = other.getOrderCount();
            if(this$orderCount == null) {
               if(other$orderCount != null) {
                  return false;
               }
            } else if(!this$orderCount.equals(other$orderCount)) {
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

            Object this$integral = this.getIntegral();
            Object other$integral = other.getIntegral();
            if(this$integral == null) {
               if(other$integral != null) {
                  return false;
               }
            } else if(!this$integral.equals(other$integral)) {
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

            Object this$saleTax = this.getSaleTax();
            Object other$saleTax = other.getSaleTax();
            if(this$saleTax == null) {
               if(other$saleTax != null) {
                  return false;
               }
            } else if(!this$saleTax.equals(other$saleTax)) {
               return false;
            }

            Object this$sumAt = this.getSumAt();
            Object other$sumAt = other.getSumAt();
            if(this$sumAt == null) {
               if(other$sumAt != null) {
                  return false;
               }
            } else if(!this$sumAt.equals(other$sumAt)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof SettlementSumOfDailyDto;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $sellerId = this.getSellerId();
      result = result * 31 + ($sellerId == null?0:$sellerId.hashCode());
      Object $sellerName = this.getSellerName();
      result = result * 31 + ($sellerName == null?0:$sellerName.hashCode());
      Object $shopId = this.getShopId();
      result = result * 31 + ($shopId == null?0:$shopId.hashCode());
      Object $shopName = this.getShopName();
      result = result * 31 + ($shopName == null?0:$shopName.hashCode());
      Object $orderCount = this.getOrderCount();
      result = result * 31 + ($orderCount == null?0:$orderCount.hashCode());
      Object $fee = this.getFee();
      result = result * 31 + ($fee == null?0:$fee.hashCode());
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
      Object $refundFee = this.getRefundFee();
      result = result * 31 + ($refundFee == null?0:$refundFee.hashCode());
      Object $commission = this.getCommission();
      result = result * 31 + ($commission == null?0:$commission.hashCode());
      Object $integral = this.getIntegral();
      result = result * 31 + ($integral == null?0:$integral.hashCode());
      Object $thirdPartyFee = this.getThirdPartyFee();
      result = result * 31 + ($thirdPartyFee == null?0:$thirdPartyFee.hashCode());
      Object $saleTax = this.getSaleTax();
      result = result * 31 + ($saleTax == null?0:$saleTax.hashCode());
      Object $sumAt = this.getSumAt();
      result = result * 31 + ($sumAt == null?0:$sumAt.hashCode());
      return result;
   }

   public Long getSellerId() {
      return this.sellerId;
   }

   public void setSellerId(Long sellerId) {
      this.sellerId = sellerId;
   }

   public String getSellerName() {
      return this.sellerName;
   }

   public void setSellerName(String sellerName) {
      this.sellerName = sellerName;
   }

   public Long getShopId() {
      return this.shopId;
   }

   public void setShopId(Long shopId) {
      this.shopId = shopId;
   }

   public String getShopName() {
      return this.shopName;
   }

   public void setShopName(String shopName) {
      this.shopName = shopName;
   }

   public Integer getOrderCount() {
      return this.orderCount;
   }

   public void setOrderCount(Integer orderCount) {
      this.orderCount = orderCount;
   }

   public Long getFee() {
      return this.fee;
   }

   public void setFee(Long fee) {
      this.fee = fee;
   }

   public Long getOriginFee() {
      return this.originFee;
   }

   public void setOriginFee(Long originFee) {
      this.originFee = originFee;
   }

   public Long getSellerDiscount() {
      return this.sellerDiscount;
   }

   public void setSellerDiscount(Long sellerDiscount) {
      this.sellerDiscount = sellerDiscount;
   }

   public Long getShipFee() {
      return this.shipFee;
   }

   public void setShipFee(Long shipFee) {
      this.shipFee = shipFee;
   }

   public Long getReceivable() {
      return this.receivable;
   }

   public void setReceivable(Long receivable) {
      this.receivable = receivable;
   }

   public Long getPlatformDiscount() {
      return this.platformDiscount;
   }

   public void setPlatformDiscount(Long platformDiscount) {
      this.platformDiscount = platformDiscount;
   }

   public Long getRefundFee() {
      return this.refundFee;
   }

   public void setRefundFee(Long refundFee) {
      this.refundFee = refundFee;
   }

   public Long getCommission() {
      return this.commission;
   }

   public void setCommission(Long commission) {
      this.commission = commission;
   }

   public Long getIntegral() {
      return this.integral;
   }

   public void setIntegral(Long integral) {
      this.integral = integral;
   }

   public Long getThirdPartyFee() {
      return this.thirdPartyFee;
   }

   public void setThirdPartyFee(Long thirdPartyFee) {
      this.thirdPartyFee = thirdPartyFee;
   }

   public Long getSaleTax() {
      return this.saleTax;
   }

   public void setSaleTax(Long saleTax) {
      this.saleTax = saleTax;
   }

   public Date getSumAt() {
      return this.sumAt;
   }

   public void setSumAt(Date sumAt) {
      this.sumAt = sumAt;
   }
}
