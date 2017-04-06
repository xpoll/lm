package cn.blmdz.wolf.pay.dto;

import java.io.Serializable;

public class ThirdPartyFeeDto implements Serializable {
   private static final long serialVersionUID = -6399410818082344970L;
   private Long fee;
   private Long thirdPartyFee;
   private String thirdPartyRate;
   private String channel;

   public Long getFee() {
      return this.fee;
   }

   public Long getThirdPartyFee() {
      return this.thirdPartyFee;
   }

   public String getThirdPartyRate() {
      return this.thirdPartyRate;
   }

   public String getChannel() {
      return this.channel;
   }

   public void setFee(Long fee) {
      this.fee = fee;
   }

   public void setThirdPartyFee(Long thirdPartyFee) {
      this.thirdPartyFee = thirdPartyFee;
   }

   public void setThirdPartyRate(String thirdPartyRate) {
      this.thirdPartyRate = thirdPartyRate;
   }

   public void setChannel(String channel) {
      this.channel = channel;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ThirdPartyFeeDto)) {
         return false;
      } else {
         ThirdPartyFeeDto other = (ThirdPartyFeeDto)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$fee = this.getFee();
            Object other$fee = other.getFee();
            if(this$fee == null) {
               if(other$fee != null) {
                  return false;
               }
            } else if(!this$fee.equals(other$fee)) {
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

            Object this$thirdPartyRate = this.getThirdPartyRate();
            Object other$thirdPartyRate = other.getThirdPartyRate();
            if(this$thirdPartyRate == null) {
               if(other$thirdPartyRate != null) {
                  return false;
               }
            } else if(!this$thirdPartyRate.equals(other$thirdPartyRate)) {
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

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof ThirdPartyFeeDto;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $fee = this.getFee();
      result = result * 31 + ($fee == null?0:$fee.hashCode());
      Object $thirdPartyFee = this.getThirdPartyFee();
      result = result * 31 + ($thirdPartyFee == null?0:$thirdPartyFee.hashCode());
      Object $thirdPartyRate = this.getThirdPartyRate();
      result = result * 31 + ($thirdPartyRate == null?0:$thirdPartyRate.hashCode());
      Object $channel = this.getChannel();
      result = result * 31 + ($channel == null?0:$channel.hashCode());
      return result;
   }

   public String toString() {
      return "ThirdPartyFeeDto(fee=" + this.getFee() + ", thirdPartyFee=" + this.getThirdPartyFee() + ", thirdPartyRate=" + this.getThirdPartyRate() + ", channel=" + this.getChannel() + ")";
   }
}
