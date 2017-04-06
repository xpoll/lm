package cn.blmdz.wolf.order.model;

import java.io.Serializable;

import cn.blmdz.wolf.order.model.Order;

public class SkuOrder extends Order implements Serializable {
   private static final long serialVersionUID = 4458707475241223633L;
   private Integer originFee;
   private Integer fee;
   private Integer discount;
   private Integer shipFee;
   private Integer shipFeeDiscount;
   private Integer integral;
   private Integer balance;
   private Integer saleTax;
   private Integer shipFeeSaleTax;
   private Long buyerId;
   private String buyerName;
   private String outBuyerId;
   private Long shopId;
   private String shopName;
   private String outShopId;
   private Long companyId;
   private Long skuId;
   private String outSkuId;
   private String skuAttributes;
   private Long itemId;
   private Long itemSnapshotId;
   private String itemName;
   private String itemImage;
   private String outItemId;
   private Integer quantity;
   private Integer channel;
   private Integer payType;
   private Integer deliverType;
   private Integer hasRefund;

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof SkuOrder)) {
         return false;
      } else {
         SkuOrder other = (SkuOrder)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$originFee = this.getOriginFee();
            Object other$originFee = other.getOriginFee();
            if(this$originFee == null) {
               if(other$originFee != null) {
                  return false;
               }
            } else if(!this$originFee.equals(other$originFee)) {
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

            Object this$discount = this.getDiscount();
            Object other$discount = other.getDiscount();
            if(this$discount == null) {
               if(other$discount != null) {
                  return false;
               }
            } else if(!this$discount.equals(other$discount)) {
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

            Object this$shipFeeSaleTax = this.getShipFeeSaleTax();
            Object other$shipFeeSaleTax = other.getShipFeeSaleTax();
            if(this$shipFeeSaleTax == null) {
               if(other$shipFeeSaleTax != null) {
                  return false;
               }
            } else if(!this$shipFeeSaleTax.equals(other$shipFeeSaleTax)) {
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

            Object this$outBuyerId = this.getOutBuyerId();
            Object other$outBuyerId = other.getOutBuyerId();
            if(this$outBuyerId == null) {
               if(other$outBuyerId != null) {
                  return false;
               }
            } else if(!this$outBuyerId.equals(other$outBuyerId)) {
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

            Object this$outShopId = this.getOutShopId();
            Object other$outShopId = other.getOutShopId();
            if(this$outShopId == null) {
               if(other$outShopId != null) {
                  return false;
               }
            } else if(!this$outShopId.equals(other$outShopId)) {
               return false;
            }

            Object this$companyId = this.getCompanyId();
            Object other$companyId = other.getCompanyId();
            if(this$companyId == null) {
               if(other$companyId != null) {
                  return false;
               }
            } else if(!this$companyId.equals(other$companyId)) {
               return false;
            }

            Object this$skuId = this.getSkuId();
            Object other$skuId = other.getSkuId();
            if(this$skuId == null) {
               if(other$skuId != null) {
                  return false;
               }
            } else if(!this$skuId.equals(other$skuId)) {
               return false;
            }

            Object this$outSkuId = this.getOutSkuId();
            Object other$outSkuId = other.getOutSkuId();
            if(this$outSkuId == null) {
               if(other$outSkuId != null) {
                  return false;
               }
            } else if(!this$outSkuId.equals(other$outSkuId)) {
               return false;
            }

            Object this$skuAttributes = this.getSkuAttributes();
            Object other$skuAttributes = other.getSkuAttributes();
            if(this$skuAttributes == null) {
               if(other$skuAttributes != null) {
                  return false;
               }
            } else if(!this$skuAttributes.equals(other$skuAttributes)) {
               return false;
            }

            Object this$itemId = this.getItemId();
            Object other$itemId = other.getItemId();
            if(this$itemId == null) {
               if(other$itemId != null) {
                  return false;
               }
            } else if(!this$itemId.equals(other$itemId)) {
               return false;
            }

            Object this$itemSnapshotId = this.getItemSnapshotId();
            Object other$itemSnapshotId = other.getItemSnapshotId();
            if(this$itemSnapshotId == null) {
               if(other$itemSnapshotId != null) {
                  return false;
               }
            } else if(!this$itemSnapshotId.equals(other$itemSnapshotId)) {
               return false;
            }

            Object this$itemName = this.getItemName();
            Object other$itemName = other.getItemName();
            if(this$itemName == null) {
               if(other$itemName != null) {
                  return false;
               }
            } else if(!this$itemName.equals(other$itemName)) {
               return false;
            }

            Object this$itemImage = this.getItemImage();
            Object other$itemImage = other.getItemImage();
            if(this$itemImage == null) {
               if(other$itemImage != null) {
                  return false;
               }
            } else if(!this$itemImage.equals(other$itemImage)) {
               return false;
            }

            Object this$outItemId = this.getOutItemId();
            Object other$outItemId = other.getOutItemId();
            if(this$outItemId == null) {
               if(other$outItemId != null) {
                  return false;
               }
            } else if(!this$outItemId.equals(other$outItemId)) {
               return false;
            }

            Object this$quantity = this.getQuantity();
            Object other$quantity = other.getQuantity();
            if(this$quantity == null) {
               if(other$quantity != null) {
                  return false;
               }
            } else if(!this$quantity.equals(other$quantity)) {
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

            Object this$payType = this.getPayType();
            Object other$payType = other.getPayType();
            if(this$payType == null) {
               if(other$payType != null) {
                  return false;
               }
            } else if(!this$payType.equals(other$payType)) {
               return false;
            }

            Object this$deliverType = this.getDeliverType();
            Object other$deliverType = other.getDeliverType();
            if(this$deliverType == null) {
               if(other$deliverType != null) {
                  return false;
               }
            } else if(!this$deliverType.equals(other$deliverType)) {
               return false;
            }

            Object this$hasRefund = this.getHasRefund();
            Object other$hasRefund = other.getHasRefund();
            if(this$hasRefund == null) {
               if(other$hasRefund != null) {
                  return false;
               }
            } else if(!this$hasRefund.equals(other$hasRefund)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof SkuOrder;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $originFee = this.getOriginFee();
      result = result * 59 + ($originFee == null?0:$originFee.hashCode());
      Object $fee = this.getFee();
      result = result * 59 + ($fee == null?0:$fee.hashCode());
      Object $discount = this.getDiscount();
      result = result * 59 + ($discount == null?0:$discount.hashCode());
      Object $shipFee = this.getShipFee();
      result = result * 59 + ($shipFee == null?0:$shipFee.hashCode());
      Object $shipFeeDiscount = this.getShipFeeDiscount();
      result = result * 59 + ($shipFeeDiscount == null?0:$shipFeeDiscount.hashCode());
      Object $integral = this.getIntegral();
      result = result * 59 + ($integral == null?0:$integral.hashCode());
      Object $balance = this.getBalance();
      result = result * 59 + ($balance == null?0:$balance.hashCode());
      Object $saleTax = this.getSaleTax();
      result = result * 59 + ($saleTax == null?0:$saleTax.hashCode());
      Object $shipFeeSaleTax = this.getShipFeeSaleTax();
      result = result * 59 + ($shipFeeSaleTax == null?0:$shipFeeSaleTax.hashCode());
      Object $buyerId = this.getBuyerId();
      result = result * 59 + ($buyerId == null?0:$buyerId.hashCode());
      Object $buyerName = this.getBuyerName();
      result = result * 59 + ($buyerName == null?0:$buyerName.hashCode());
      Object $outBuyerId = this.getOutBuyerId();
      result = result * 59 + ($outBuyerId == null?0:$outBuyerId.hashCode());
      Object $shopId = this.getShopId();
      result = result * 59 + ($shopId == null?0:$shopId.hashCode());
      Object $shopName = this.getShopName();
      result = result * 59 + ($shopName == null?0:$shopName.hashCode());
      Object $outShopId = this.getOutShopId();
      result = result * 59 + ($outShopId == null?0:$outShopId.hashCode());
      Object $companyId = this.getCompanyId();
      result = result * 59 + ($companyId == null?0:$companyId.hashCode());
      Object $skuId = this.getSkuId();
      result = result * 59 + ($skuId == null?0:$skuId.hashCode());
      Object $outSkuId = this.getOutSkuId();
      result = result * 59 + ($outSkuId == null?0:$outSkuId.hashCode());
      Object $skuAttributes = this.getSkuAttributes();
      result = result * 59 + ($skuAttributes == null?0:$skuAttributes.hashCode());
      Object $itemId = this.getItemId();
      result = result * 59 + ($itemId == null?0:$itemId.hashCode());
      Object $itemSnapshotId = this.getItemSnapshotId();
      result = result * 59 + ($itemSnapshotId == null?0:$itemSnapshotId.hashCode());
      Object $itemName = this.getItemName();
      result = result * 59 + ($itemName == null?0:$itemName.hashCode());
      Object $itemImage = this.getItemImage();
      result = result * 59 + ($itemImage == null?0:$itemImage.hashCode());
      Object $outItemId = this.getOutItemId();
      result = result * 59 + ($outItemId == null?0:$outItemId.hashCode());
      Object $quantity = this.getQuantity();
      result = result * 59 + ($quantity == null?0:$quantity.hashCode());
      Object $channel = this.getChannel();
      result = result * 59 + ($channel == null?0:$channel.hashCode());
      Object $payType = this.getPayType();
      result = result * 59 + ($payType == null?0:$payType.hashCode());
      Object $deliverType = this.getDeliverType();
      result = result * 59 + ($deliverType == null?0:$deliverType.hashCode());
      Object $hasRefund = this.getHasRefund();
      result = result * 59 + ($hasRefund == null?0:$hasRefund.hashCode());
      return result;
   }

   public Integer getOriginFee() {
      return this.originFee;
   }

   public Integer getFee() {
      return this.fee;
   }

   public Integer getDiscount() {
      return this.discount;
   }

   public Integer getShipFee() {
      return this.shipFee;
   }

   public Integer getShipFeeDiscount() {
      return this.shipFeeDiscount;
   }

   public Integer getIntegral() {
      return this.integral;
   }

   public Integer getBalance() {
      return this.balance;
   }

   public Integer getSaleTax() {
      return this.saleTax;
   }

   public Integer getShipFeeSaleTax() {
      return this.shipFeeSaleTax;
   }

   public Long getBuyerId() {
      return this.buyerId;
   }

   public String getBuyerName() {
      return this.buyerName;
   }

   public String getOutBuyerId() {
      return this.outBuyerId;
   }

   public Long getShopId() {
      return this.shopId;
   }

   public String getShopName() {
      return this.shopName;
   }

   public String getOutShopId() {
      return this.outShopId;
   }

   public Long getCompanyId() {
      return this.companyId;
   }

   public Long getSkuId() {
      return this.skuId;
   }

   public String getOutSkuId() {
      return this.outSkuId;
   }

   public String getSkuAttributes() {
      return this.skuAttributes;
   }

   public Long getItemId() {
      return this.itemId;
   }

   public Long getItemSnapshotId() {
      return this.itemSnapshotId;
   }

   public String getItemName() {
      return this.itemName;
   }

   public String getItemImage() {
      return this.itemImage;
   }

   public String getOutItemId() {
      return this.outItemId;
   }

   public Integer getQuantity() {
      return this.quantity;
   }

   public Integer getChannel() {
      return this.channel;
   }

   public Integer getPayType() {
      return this.payType;
   }

   public Integer getDeliverType() {
      return this.deliverType;
   }

   public Integer getHasRefund() {
      return this.hasRefund;
   }

   public void setOriginFee(Integer originFee) {
      this.originFee = originFee;
   }

   public void setFee(Integer fee) {
      this.fee = fee;
   }

   public void setDiscount(Integer discount) {
      this.discount = discount;
   }

   public void setShipFee(Integer shipFee) {
      this.shipFee = shipFee;
   }

   public void setShipFeeDiscount(Integer shipFeeDiscount) {
      this.shipFeeDiscount = shipFeeDiscount;
   }

   public void setIntegral(Integer integral) {
      this.integral = integral;
   }

   public void setBalance(Integer balance) {
      this.balance = balance;
   }

   public void setSaleTax(Integer saleTax) {
      this.saleTax = saleTax;
   }

   public void setShipFeeSaleTax(Integer shipFeeSaleTax) {
      this.shipFeeSaleTax = shipFeeSaleTax;
   }

   public void setBuyerId(Long buyerId) {
      this.buyerId = buyerId;
   }

   public void setBuyerName(String buyerName) {
      this.buyerName = buyerName;
   }

   public void setOutBuyerId(String outBuyerId) {
      this.outBuyerId = outBuyerId;
   }

   public void setShopId(Long shopId) {
      this.shopId = shopId;
   }

   public void setShopName(String shopName) {
      this.shopName = shopName;
   }

   public void setOutShopId(String outShopId) {
      this.outShopId = outShopId;
   }

   public void setCompanyId(Long companyId) {
      this.companyId = companyId;
   }

   public void setSkuId(Long skuId) {
      this.skuId = skuId;
   }

   public void setOutSkuId(String outSkuId) {
      this.outSkuId = outSkuId;
   }

   public void setSkuAttributes(String skuAttributes) {
      this.skuAttributes = skuAttributes;
   }

   public void setItemId(Long itemId) {
      this.itemId = itemId;
   }

   public void setItemSnapshotId(Long itemSnapshotId) {
      this.itemSnapshotId = itemSnapshotId;
   }

   public void setItemName(String itemName) {
      this.itemName = itemName;
   }

   public void setItemImage(String itemImage) {
      this.itemImage = itemImage;
   }

   public void setOutItemId(String outItemId) {
      this.outItemId = outItemId;
   }

   public void setQuantity(Integer quantity) {
      this.quantity = quantity;
   }

   public void setChannel(Integer channel) {
      this.channel = channel;
   }

   public void setPayType(Integer payType) {
      this.payType = payType;
   }

   public void setDeliverType(Integer deliverType) {
      this.deliverType = deliverType;
   }

   public void setHasRefund(Integer hasRefund) {
      this.hasRefund = hasRefund;
   }

   public String toString() {
      return "SkuOrder(originFee=" + this.getOriginFee() + ", fee=" + this.getFee() + ", discount=" + this.getDiscount() + ", shipFee=" + this.getShipFee() + ", shipFeeDiscount=" + this.getShipFeeDiscount() + ", integral=" + this.getIntegral() + ", balance=" + this.getBalance() + ", saleTax=" + this.getSaleTax() + ", shipFeeSaleTax=" + this.getShipFeeSaleTax() + ", buyerId=" + this.getBuyerId() + ", buyerName=" + this.getBuyerName() + ", outBuyerId=" + this.getOutBuyerId() + ", shopId=" + this.getShopId() + ", shopName=" + this.getShopName() + ", outShopId=" + this.getOutShopId() + ", companyId=" + this.getCompanyId() + ", skuId=" + this.getSkuId() + ", outSkuId=" + this.getOutSkuId() + ", skuAttributes=" + this.getSkuAttributes() + ", itemId=" + this.getItemId() + ", itemSnapshotId=" + this.getItemSnapshotId() + ", itemName=" + this.getItemName() + ", itemImage=" + this.getItemImage() + ", outItemId=" + this.getOutItemId() + ", quantity=" + this.getQuantity() + ", channel=" + this.getChannel() + ", payType=" + this.getPayType() + ", deliverType=" + this.getDeliverType() + ", hasRefund=" + this.getHasRefund() + ")";
   }
}
