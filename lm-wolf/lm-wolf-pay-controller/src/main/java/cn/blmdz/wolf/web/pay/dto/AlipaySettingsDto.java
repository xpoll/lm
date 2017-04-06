package cn.blmdz.wolf.web.pay.dto;

import java.io.Serializable;

public class AlipaySettingsDto implements Serializable {
   private String pid;
   private String key;
   private String account;
   private String gateway;
   private String wapGateway;
   private String notifyUrl;
   private String returnUrl;
   private String refundNotifyUrl;
   private String refundType;
   private String wapPayVersion;

   public String getPid() {
      return this.pid;
   }

   public String getKey() {
      return this.key;
   }

   public String getAccount() {
      return this.account;
   }

   public String getGateway() {
      return this.gateway;
   }

   public String getWapGateway() {
      return this.wapGateway;
   }

   public String getNotifyUrl() {
      return this.notifyUrl;
   }

   public String getReturnUrl() {
      return this.returnUrl;
   }

   public String getRefundNotifyUrl() {
      return this.refundNotifyUrl;
   }

   public String getRefundType() {
      return this.refundType;
   }

   public String getWapPayVersion() {
      return this.wapPayVersion;
   }

   public void setPid(String pid) {
      this.pid = pid;
   }

   public void setKey(String key) {
      this.key = key;
   }

   public void setAccount(String account) {
      this.account = account;
   }

   public void setGateway(String gateway) {
      this.gateway = gateway;
   }

   public void setWapGateway(String wapGateway) {
      this.wapGateway = wapGateway;
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

   public void setRefundType(String refundType) {
      this.refundType = refundType;
   }

   public void setWapPayVersion(String wapPayVersion) {
      this.wapPayVersion = wapPayVersion;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof AlipaySettingsDto)) {
         return false;
      } else {
         AlipaySettingsDto other = (AlipaySettingsDto)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$pid = this.getPid();
            Object other$pid = other.getPid();
            if(this$pid == null) {
               if(other$pid != null) {
                  return false;
               }
            } else if(!this$pid.equals(other$pid)) {
               return false;
            }

            Object this$key = this.getKey();
            Object other$key = other.getKey();
            if(this$key == null) {
               if(other$key != null) {
                  return false;
               }
            } else if(!this$key.equals(other$key)) {
               return false;
            }

            Object this$account = this.getAccount();
            Object other$account = other.getAccount();
            if(this$account == null) {
               if(other$account != null) {
                  return false;
               }
            } else if(!this$account.equals(other$account)) {
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

            Object this$wapGateway = this.getWapGateway();
            Object other$wapGateway = other.getWapGateway();
            if(this$wapGateway == null) {
               if(other$wapGateway != null) {
                  return false;
               }
            } else if(!this$wapGateway.equals(other$wapGateway)) {
               return false;
            }

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

            Object this$refundType = this.getRefundType();
            Object other$refundType = other.getRefundType();
            if(this$refundType == null) {
               if(other$refundType != null) {
                  return false;
               }
            } else if(!this$refundType.equals(other$refundType)) {
               return false;
            }

            Object this$wapPayVersion = this.getWapPayVersion();
            Object other$wapPayVersion = other.getWapPayVersion();
            if(this$wapPayVersion == null) {
               if(other$wapPayVersion != null) {
                  return false;
               }
            } else if(!this$wapPayVersion.equals(other$wapPayVersion)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof AlipaySettingsDto;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $pid = this.getPid();
      result = result * 31 + ($pid == null?0:$pid.hashCode());
      Object $key = this.getKey();
      result = result * 31 + ($key == null?0:$key.hashCode());
      Object $account = this.getAccount();
      result = result * 31 + ($account == null?0:$account.hashCode());
      Object $gateway = this.getGateway();
      result = result * 31 + ($gateway == null?0:$gateway.hashCode());
      Object $wapGateway = this.getWapGateway();
      result = result * 31 + ($wapGateway == null?0:$wapGateway.hashCode());
      Object $notifyUrl = this.getNotifyUrl();
      result = result * 31 + ($notifyUrl == null?0:$notifyUrl.hashCode());
      Object $returnUrl = this.getReturnUrl();
      result = result * 31 + ($returnUrl == null?0:$returnUrl.hashCode());
      Object $refundNotifyUrl = this.getRefundNotifyUrl();
      result = result * 31 + ($refundNotifyUrl == null?0:$refundNotifyUrl.hashCode());
      Object $refundType = this.getRefundType();
      result = result * 31 + ($refundType == null?0:$refundType.hashCode());
      Object $wapPayVersion = this.getWapPayVersion();
      result = result * 31 + ($wapPayVersion == null?0:$wapPayVersion.hashCode());
      return result;
   }

   public String toString() {
      return "AlipaySettingsDto(pid=" + this.getPid() + ", key=" + this.getKey() + ", account=" + this.getAccount() + ", gateway=" + this.getGateway() + ", wapGateway=" + this.getWapGateway() + ", notifyUrl=" + this.getNotifyUrl() + ", returnUrl=" + this.getReturnUrl() + ", refundNotifyUrl=" + this.getRefundNotifyUrl() + ", refundType=" + this.getRefundType() + ", wapPayVersion=" + this.getWapPayVersion() + ")";
   }
}
