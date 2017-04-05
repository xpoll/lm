package io.terminus.parana.pay.enums;

import io.terminus.parana.pay.model.PayChannel;
import io.terminus.parana.pay.model.PayStage;
import io.terminus.parana.pay.model.TradePay;
import java.io.Serializable;
import java.util.List;

public class PayInfoDto implements Serializable {
   private TradePay tradePay;
   private PayStage payStage;
   private List allPayStages;
   private PayChannel payChannel;
   private List allPayChannels;

   public TradePay getTradePay() {
      return this.tradePay;
   }

   public PayStage getPayStage() {
      return this.payStage;
   }

   public List getAllPayStages() {
      return this.allPayStages;
   }

   public PayChannel getPayChannel() {
      return this.payChannel;
   }

   public List getAllPayChannels() {
      return this.allPayChannels;
   }

   public void setTradePay(TradePay tradePay) {
      this.tradePay = tradePay;
   }

   public void setPayStage(PayStage payStage) {
      this.payStage = payStage;
   }

   public void setAllPayStages(List allPayStages) {
      this.allPayStages = allPayStages;
   }

   public void setPayChannel(PayChannel payChannel) {
      this.payChannel = payChannel;
   }

   public void setAllPayChannels(List allPayChannels) {
      this.allPayChannels = allPayChannels;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof PayInfoDto)) {
         return false;
      } else {
         PayInfoDto other = (PayInfoDto)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$tradePay = this.getTradePay();
            Object other$tradePay = other.getTradePay();
            if(this$tradePay == null) {
               if(other$tradePay != null) {
                  return false;
               }
            } else if(!this$tradePay.equals(other$tradePay)) {
               return false;
            }

            Object this$payStage = this.getPayStage();
            Object other$payStage = other.getPayStage();
            if(this$payStage == null) {
               if(other$payStage != null) {
                  return false;
               }
            } else if(!this$payStage.equals(other$payStage)) {
               return false;
            }

            Object this$allPayStages = this.getAllPayStages();
            Object other$allPayStages = other.getAllPayStages();
            if(this$allPayStages == null) {
               if(other$allPayStages != null) {
                  return false;
               }
            } else if(!this$allPayStages.equals(other$allPayStages)) {
               return false;
            }

            Object this$payChannel = this.getPayChannel();
            Object other$payChannel = other.getPayChannel();
            if(this$payChannel == null) {
               if(other$payChannel != null) {
                  return false;
               }
            } else if(!this$payChannel.equals(other$payChannel)) {
               return false;
            }

            Object this$allPayChannels = this.getAllPayChannels();
            Object other$allPayChannels = other.getAllPayChannels();
            if(this$allPayChannels == null) {
               if(other$allPayChannels != null) {
                  return false;
               }
            } else if(!this$allPayChannels.equals(other$allPayChannels)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof PayInfoDto;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $tradePay = this.getTradePay();
      result = result * 31 + ($tradePay == null?0:$tradePay.hashCode());
      Object $payStage = this.getPayStage();
      result = result * 31 + ($payStage == null?0:$payStage.hashCode());
      Object $allPayStages = this.getAllPayStages();
      result = result * 31 + ($allPayStages == null?0:$allPayStages.hashCode());
      Object $payChannel = this.getPayChannel();
      result = result * 31 + ($payChannel == null?0:$payChannel.hashCode());
      Object $allPayChannels = this.getAllPayChannels();
      result = result * 31 + ($allPayChannels == null?0:$allPayChannels.hashCode());
      return result;
   }

   public String toString() {
      return "PayInfoDto(tradePay=" + this.getTradePay() + ", payStage=" + this.getPayStage() + ", allPayStages=" + this.getAllPayStages() + ", payChannel=" + this.getPayChannel() + ", allPayChannels=" + this.getAllPayChannels() + ")";
   }
}
