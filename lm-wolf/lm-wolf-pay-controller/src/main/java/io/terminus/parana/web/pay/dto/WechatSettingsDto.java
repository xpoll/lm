package io.terminus.parana.web.pay.dto;

import java.io.Serializable;

public class WechatSettingsDto implements Serializable {
   private String caFilePath;
   private String certFilePath;
   private String notifyUrl;
   private String returnUrl;
   private String imageBaseUrl;
   private String token;
   private String publicName;

   public String getCaFilePath() {
      return this.caFilePath;
   }

   public String getCertFilePath() {
      return this.certFilePath;
   }

   public String getNotifyUrl() {
      return this.notifyUrl;
   }

   public String getReturnUrl() {
      return this.returnUrl;
   }

   public String getImageBaseUrl() {
      return this.imageBaseUrl;
   }

   public String getToken() {
      return this.token;
   }

   public String getPublicName() {
      return this.publicName;
   }

   public void setCaFilePath(String caFilePath) {
      this.caFilePath = caFilePath;
   }

   public void setCertFilePath(String certFilePath) {
      this.certFilePath = certFilePath;
   }

   public void setNotifyUrl(String notifyUrl) {
      this.notifyUrl = notifyUrl;
   }

   public void setReturnUrl(String returnUrl) {
      this.returnUrl = returnUrl;
   }

   public void setImageBaseUrl(String imageBaseUrl) {
      this.imageBaseUrl = imageBaseUrl;
   }

   public void setToken(String token) {
      this.token = token;
   }

   public void setPublicName(String publicName) {
      this.publicName = publicName;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof WechatSettingsDto)) {
         return false;
      } else {
         WechatSettingsDto other = (WechatSettingsDto)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$caFilePath = this.getCaFilePath();
            Object other$caFilePath = other.getCaFilePath();
            if(this$caFilePath == null) {
               if(other$caFilePath != null) {
                  return false;
               }
            } else if(!this$caFilePath.equals(other$caFilePath)) {
               return false;
            }

            Object this$certFilePath = this.getCertFilePath();
            Object other$certFilePath = other.getCertFilePath();
            if(this$certFilePath == null) {
               if(other$certFilePath != null) {
                  return false;
               }
            } else if(!this$certFilePath.equals(other$certFilePath)) {
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

            Object this$imageBaseUrl = this.getImageBaseUrl();
            Object other$imageBaseUrl = other.getImageBaseUrl();
            if(this$imageBaseUrl == null) {
               if(other$imageBaseUrl != null) {
                  return false;
               }
            } else if(!this$imageBaseUrl.equals(other$imageBaseUrl)) {
               return false;
            }

            Object this$token = this.getToken();
            Object other$token = other.getToken();
            if(this$token == null) {
               if(other$token != null) {
                  return false;
               }
            } else if(!this$token.equals(other$token)) {
               return false;
            }

            Object this$publicName = this.getPublicName();
            Object other$publicName = other.getPublicName();
            if(this$publicName == null) {
               if(other$publicName != null) {
                  return false;
               }
            } else if(!this$publicName.equals(other$publicName)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof WechatSettingsDto;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $caFilePath = this.getCaFilePath();
      result = result * 31 + ($caFilePath == null?0:$caFilePath.hashCode());
      Object $certFilePath = this.getCertFilePath();
      result = result * 31 + ($certFilePath == null?0:$certFilePath.hashCode());
      Object $notifyUrl = this.getNotifyUrl();
      result = result * 31 + ($notifyUrl == null?0:$notifyUrl.hashCode());
      Object $returnUrl = this.getReturnUrl();
      result = result * 31 + ($returnUrl == null?0:$returnUrl.hashCode());
      Object $imageBaseUrl = this.getImageBaseUrl();
      result = result * 31 + ($imageBaseUrl == null?0:$imageBaseUrl.hashCode());
      Object $token = this.getToken();
      result = result * 31 + ($token == null?0:$token.hashCode());
      Object $publicName = this.getPublicName();
      result = result * 31 + ($publicName == null?0:$publicName.hashCode());
      return result;
   }

   public String toString() {
      return "WechatSettingsDto(caFilePath=" + this.getCaFilePath() + ", certFilePath=" + this.getCertFilePath() + ", notifyUrl=" + this.getNotifyUrl() + ", returnUrl=" + this.getReturnUrl() + ", imageBaseUrl=" + this.getImageBaseUrl() + ", token=" + this.getToken() + ", publicName=" + this.getPublicName() + ")";
   }
}
