package cn.blmdz.wolf.pay.dto;

import java.io.Serializable;

import cn.blmdz.wolf.pay.model.PayStage;

public class OrderStageDto implements Serializable {
   private static final long serialVersionUID = -1150994564164474124L;
   private Long orderId;
   private PayStage payStage;

   public Long getOrderId() {
      return this.orderId;
   }

   public PayStage getPayStage() {
      return this.payStage;
   }

   public void setOrderId(Long orderId) {
      this.orderId = orderId;
   }

   public void setPayStage(PayStage payStage) {
      this.payStage = payStage;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof OrderStageDto)) {
         return false;
      } else {
         OrderStageDto other = (OrderStageDto)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$orderId = this.getOrderId();
            Object other$orderId = other.getOrderId();
            if(this$orderId == null) {
               if(other$orderId != null) {
                  return false;
               }
            } else if(!this$orderId.equals(other$orderId)) {
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

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof OrderStageDto;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $orderId = this.getOrderId();
      result = result * 31 + ($orderId == null?0:$orderId.hashCode());
      Object $payStage = this.getPayStage();
      result = result * 31 + ($payStage == null?0:$payStage.hashCode());
      return result;
   }

   public String toString() {
      return "OrderStageDto(orderId=" + this.getOrderId() + ", payStage=" + this.getPayStage() + ")";
   }
}
