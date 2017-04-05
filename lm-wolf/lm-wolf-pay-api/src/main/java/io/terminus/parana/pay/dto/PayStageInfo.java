package io.terminus.parana.pay.dto;

import java.io.Serializable;
import java.util.Date;

public class PayStageInfo implements Serializable {
   private static final long serialVersionUID = 872573968235665722L;
   private Integer currentStage;
   private Integer fee;
   private Date repayAt;

   public Integer getCurrentStage() {
      return this.currentStage;
   }

   public Integer getFee() {
      return this.fee;
   }

   public Date getRepayAt() {
      return this.repayAt;
   }

   public void setCurrentStage(Integer currentStage) {
      this.currentStage = currentStage;
   }

   public void setFee(Integer fee) {
      this.fee = fee;
   }

   public void setRepayAt(Date repayAt) {
      this.repayAt = repayAt;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof PayStageInfo)) {
         return false;
      } else {
         PayStageInfo other = (PayStageInfo)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$currentStage = this.getCurrentStage();
            Object other$currentStage = other.getCurrentStage();
            if(this$currentStage == null) {
               if(other$currentStage != null) {
                  return false;
               }
            } else if(!this$currentStage.equals(other$currentStage)) {
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

            Object this$repayAt = this.getRepayAt();
            Object other$repayAt = other.getRepayAt();
            if(this$repayAt == null) {
               if(other$repayAt != null) {
                  return false;
               }
            } else if(!this$repayAt.equals(other$repayAt)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof PayStageInfo;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $currentStage = this.getCurrentStage();
      result = result * 31 + ($currentStage == null?0:$currentStage.hashCode());
      Object $fee = this.getFee();
      result = result * 31 + ($fee == null?0:$fee.hashCode());
      Object $repayAt = this.getRepayAt();
      result = result * 31 + ($repayAt == null?0:$repayAt.hashCode());
      return result;
   }

   public String toString() {
      return "PayStageInfo(currentStage=" + this.getCurrentStage() + ", fee=" + this.getFee() + ", repayAt=" + this.getRepayAt() + ")";
   }
}
