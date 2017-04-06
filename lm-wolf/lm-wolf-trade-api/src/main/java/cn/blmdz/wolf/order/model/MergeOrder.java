package cn.blmdz.wolf.order.model;

import java.io.Serializable;

import cn.blmdz.wolf.order.model.Order;

public class MergeOrder extends Order implements Serializable {
   private static final long serialVersionUID = 6527044306408455502L;
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
   private Long companyId;
   private Integer payType;
   private Integer channel;
   private Integer hasRefund;

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof MergeOrder)) {
         return false;
      } else {
         MergeOrder other = (MergeOrder)o;
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

            Object this$companyId = this.getCompanyId();
            Object other$companyId = other.getCompanyId();
            if(this$companyId == null) {
               if(other$companyId != null) {
                  return false;
               }
            } else if(!this$companyId.equals(other$companyId)) {
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

            Object this$channel = this.getChannel();
            Object other$channel = other.getChannel();
            if(this$channel == null) {
               if(other$channel != null) {
                  return false;
               }
            } else if(!this$channel.equals(other$channel)) {
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
      return other instanceof MergeOrder;
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
      Object $companyId = this.getCompanyId();
      result = result * 59 + ($companyId == null?0:$companyId.hashCode());
      Object $payType = this.getPayType();
      result = result * 59 + ($payType == null?0:$payType.hashCode());
      Object $channel = this.getChannel();
      result = result * 59 + ($channel == null?0:$channel.hashCode());
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

   public Long getCompanyId() {
      return this.companyId;
   }

   public Integer getPayType() {
      return this.payType;
   }

   public Integer getChannel() {
      return this.channel;
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

   public void setCompanyId(Long companyId) {
      this.companyId = companyId;
   }

   public void setPayType(Integer payType) {
      this.payType = payType;
   }

   public void setChannel(Integer channel) {
      this.channel = channel;
   }

   public void setHasRefund(Integer hasRefund) {
      this.hasRefund = hasRefund;
   }

   public String toString() {
      return "MergeOrder(originFee=" + this.getOriginFee() + ", fee=" + this.getFee() + ", discount=" + this.getDiscount() + ", shipFee=" + this.getShipFee() + ", shipFeeDiscount=" + this.getShipFeeDiscount() + ", integral=" + this.getIntegral() + ", balance=" + this.getBalance() + ", saleTax=" + this.getSaleTax() + ", shipFeeSaleTax=" + this.getShipFeeSaleTax() + ", buyerId=" + this.getBuyerId() + ", buyerName=" + this.getBuyerName() + ", outBuyerId=" + this.getOutBuyerId() + ", companyId=" + this.getCompanyId() + ", payType=" + this.getPayType() + ", channel=" + this.getChannel() + ", hasRefund=" + this.getHasRefund() + ")";
   }
}
