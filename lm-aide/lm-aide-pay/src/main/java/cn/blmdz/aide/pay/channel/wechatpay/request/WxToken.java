package io.terminus.lib.pay.channel.wechatpay.request;

import io.terminus.lib.pay.common.BaseToken;

public class WxToken extends BaseToken {
   private String appId;
   private String secret;
   private String mchId;
   private String paternerkey;
   private String gateway;
   private String refundGateway;
   private String dowloadBillUrl;
   private String queryRefundGateway;

   public String getAppId() {
      return this.appId;
   }

   public String getSecret() {
      return this.secret;
   }

   public String getMchId() {
      return this.mchId;
   }

   public String getPaternerkey() {
      return this.paternerkey;
   }

   public String getGateway() {
      return this.gateway;
   }

   public String getRefundGateway() {
      return this.refundGateway;
   }

   public String getDowloadBillUrl() {
      return this.dowloadBillUrl;
   }

   public String getQueryRefundGateway() {
      return this.queryRefundGateway;
   }

   public void setAppId(String appId) {
      this.appId = appId;
   }

   public void setSecret(String secret) {
      this.secret = secret;
   }

   public void setMchId(String mchId) {
      this.mchId = mchId;
   }

   public void setPaternerkey(String paternerkey) {
      this.paternerkey = paternerkey;
   }

   public void setGateway(String gateway) {
      this.gateway = gateway;
   }

   public void setRefundGateway(String refundGateway) {
      this.refundGateway = refundGateway;
   }

   public void setDowloadBillUrl(String dowloadBillUrl) {
      this.dowloadBillUrl = dowloadBillUrl;
   }

   public void setQueryRefundGateway(String queryRefundGateway) {
      this.queryRefundGateway = queryRefundGateway;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof WxToken)) {
         return false;
      } else {
         WxToken other = (WxToken)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$appId = this.getAppId();
            Object other$appId = other.getAppId();
            if(this$appId == null) {
               if(other$appId != null) {
                  return false;
               }
            } else if(!this$appId.equals(other$appId)) {
               return false;
            }

            Object this$secret = this.getSecret();
            Object other$secret = other.getSecret();
            if(this$secret == null) {
               if(other$secret != null) {
                  return false;
               }
            } else if(!this$secret.equals(other$secret)) {
               return false;
            }

            Object this$mchId = this.getMchId();
            Object other$mchId = other.getMchId();
            if(this$mchId == null) {
               if(other$mchId != null) {
                  return false;
               }
            } else if(!this$mchId.equals(other$mchId)) {
               return false;
            }

            Object this$paternerkey = this.getPaternerkey();
            Object other$paternerkey = other.getPaternerkey();
            if(this$paternerkey == null) {
               if(other$paternerkey != null) {
                  return false;
               }
            } else if(!this$paternerkey.equals(other$paternerkey)) {
               return false;
            }

            Object this$gateway = this.getGateway();
            Object other$gateway = other.getGateway();
            if(this$gateway == null) {
               if(other$gateway != null) {
                  return false;
               }
            } else if(!this$gateway.equals(other$gateway)) {
               return false;
            }

            Object this$refundGateway = this.getRefundGateway();
            Object other$refundGateway = other.getRefundGateway();
            if(this$refundGateway == null) {
               if(other$refundGateway != null) {
                  return false;
               }
            } else if(!this$refundGateway.equals(other$refundGateway)) {
               return false;
            }

            Object this$dowloadBillUrl = this.getDowloadBillUrl();
            Object other$dowloadBillUrl = other.getDowloadBillUrl();
            if(this$dowloadBillUrl == null) {
               if(other$dowloadBillUrl != null) {
                  return false;
               }
            } else if(!this$dowloadBillUrl.equals(other$dowloadBillUrl)) {
               return false;
            }

            Object this$queryRefundGateway = this.getQueryRefundGateway();
            Object other$queryRefundGateway = other.getQueryRefundGateway();
            if(this$queryRefundGateway == null) {
               if(other$queryRefundGateway != null) {
                  return false;
               }
            } else if(!this$queryRefundGateway.equals(other$queryRefundGateway)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof WxToken;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $appId = this.getAppId();
      result = result * 59 + ($appId == null?0:$appId.hashCode());
      Object $secret = this.getSecret();
      result = result * 59 + ($secret == null?0:$secret.hashCode());
      Object $mchId = this.getMchId();
      result = result * 59 + ($mchId == null?0:$mchId.hashCode());
      Object $paternerkey = this.getPaternerkey();
      result = result * 59 + ($paternerkey == null?0:$paternerkey.hashCode());
      Object $gateway = this.getGateway();
      result = result * 59 + ($gateway == null?0:$gateway.hashCode());
      Object $refundGateway = this.getRefundGateway();
      result = result * 59 + ($refundGateway == null?0:$refundGateway.hashCode());
      Object $dowloadBillUrl = this.getDowloadBillUrl();
      result = result * 59 + ($dowloadBillUrl == null?0:$dowloadBillUrl.hashCode());
      Object $queryRefundGateway = this.getQueryRefundGateway();
      result = result * 59 + ($queryRefundGateway == null?0:$queryRefundGateway.hashCode());
      return result;
   }

   public String toString() {
      return "WxToken(appId=" + this.getAppId() + ", secret=" + this.getSecret() + ", mchId=" + this.getMchId() + ", paternerkey=" + this.getPaternerkey() + ", gateway=" + this.getGateway() + ", refundGateway=" + this.getRefundGateway() + ", dowloadBillUrl=" + this.getDowloadBillUrl() + ", queryRefundGateway=" + this.getQueryRefundGateway() + ")";
   }
}
