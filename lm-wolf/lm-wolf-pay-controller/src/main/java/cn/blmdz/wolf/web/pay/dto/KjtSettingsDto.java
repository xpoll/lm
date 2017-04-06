package cn.blmdz.wolf.web.pay.dto;

import java.io.Serializable;

public class KjtSettingsDto implements Serializable {
   private String notifyUrl;
   private String returnUrl;
   private String refundNotifyUrl;

   public String getNotifyUrl() {
      return this.notifyUrl;
   }

   public String getReturnUrl() {
      return this.returnUrl;
   }

   public String getRefundNotifyUrl() {
      return this.refundNotifyUrl;
   }

   public void setNotifyUrl(String notifyUrl) {
      this.notifyUrl = notifyUrl;
   }

   public void setReturnUrl(String returnUrl) {
      this.returnUrl = returnUrl;
   }

   public void setRefundNotifyUrl(String refundNotifyUrl) {
      this.refundNotifyUrl = refundNotifyUrl;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof KjtSettingsDto)) {
         return false;
      } else {
         KjtSettingsDto other = (KjtSettingsDto)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$notifyUrl = this.getNotifyUrl();
            Object other$notifyUrl = other.getNotifyUrl();
            if(this$notifyUrl == null) {
               if(other$notifyUrl != null) {
                  return false;
               }
            } else if(!this$notifyUrl.equals(other$notifyUrl)) {
               return false;
            }

            Object this$returnUrl = this.getReturnUrl();
            Object other$returnUrl = other.getReturnUrl();
            if(this$returnUrl == null) {
               if(other$returnUrl != null) {
                  return false;
               }
            } else if(!this$returnUrl.equals(other$returnUrl)) {
               return false;
            }

            Object this$refundNotifyUrl = this.getRefundNotifyUrl();
            Object other$refundNotifyUrl = other.getRefundNotifyUrl();
            if(this$refundNotifyUrl == null) {
               if(other$refundNotifyUrl != null) {
                  return false;
               }
            } else if(!this$refundNotifyUrl.equals(other$refundNotifyUrl)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof KjtSettingsDto;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $notifyUrl = this.getNotifyUrl();
      result = result * 31 + ($notifyUrl == null?0:$notifyUrl.hashCode());
      Object $returnUrl = this.getReturnUrl();
      result = result * 31 + ($returnUrl == null?0:$returnUrl.hashCode());
      Object $refundNotifyUrl = this.getRefundNotifyUrl();
      result = result * 31 + ($refundNotifyUrl == null?0:$refundNotifyUrl.hashCode());
      return result;
   }

   public String toString() {
      return "KjtSettingsDto(notifyUrl=" + this.getNotifyUrl() + ", returnUrl=" + this.getReturnUrl() + ", refundNotifyUrl=" + this.getRefundNotifyUrl() + ")";
   }
}
