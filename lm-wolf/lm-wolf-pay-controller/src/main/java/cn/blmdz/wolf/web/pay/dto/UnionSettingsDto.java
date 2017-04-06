package cn.blmdz.wolf.web.pay.dto;

import java.io.Serializable;

public class UnionSettingsDto implements Serializable {
   private String merid;
   private String refundNotifyUrl;
   private String notifyUrl;
   private String returnUrl;
   private String certPath;
   private String certPwd;
   private String certType;
   private String validateCertDir;
   private String encryptCertPath;
   private String singleMode;
   private String transFilePatch;

   public String getMerid() {
      return this.merid;
   }

   public String getRefundNotifyUrl() {
      return this.refundNotifyUrl;
   }

   public String getNotifyUrl() {
      return this.notifyUrl;
   }

   public String getReturnUrl() {
      return this.returnUrl;
   }

   public String getCertPath() {
      return this.certPath;
   }

   public String getCertPwd() {
      return this.certPwd;
   }

   public String getCertType() {
      return this.certType;
   }

   public String getValidateCertDir() {
      return this.validateCertDir;
   }

   public String getEncryptCertPath() {
      return this.encryptCertPath;
   }

   public String getSingleMode() {
      return this.singleMode;
   }

   public String getTransFilePatch() {
      return this.transFilePatch;
   }

   public void setMerid(String merid) {
      this.merid = merid;
   }

   public void setRefundNotifyUrl(String refundNotifyUrl) {
      this.refundNotifyUrl = refundNotifyUrl;
   }

   public void setNotifyUrl(String notifyUrl) {
      this.notifyUrl = notifyUrl;
   }

   public void setReturnUrl(String returnUrl) {
      this.returnUrl = returnUrl;
   }

   public void setCertPath(String certPath) {
      this.certPath = certPath;
   }

   public void setCertPwd(String certPwd) {
      this.certPwd = certPwd;
   }

   public void setCertType(String certType) {
      this.certType = certType;
   }

   public void setValidateCertDir(String validateCertDir) {
      this.validateCertDir = validateCertDir;
   }

   public void setEncryptCertPath(String encryptCertPath) {
      this.encryptCertPath = encryptCertPath;
   }

   public void setSingleMode(String singleMode) {
      this.singleMode = singleMode;
   }

   public void setTransFilePatch(String transFilePatch) {
      this.transFilePatch = transFilePatch;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof UnionSettingsDto)) {
         return false;
      } else {
         UnionSettingsDto other = (UnionSettingsDto)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$merid = this.getMerid();
            Object other$merid = other.getMerid();
            if(this$merid == null) {
               if(other$merid != null) {
                  return false;
               }
            } else if(!this$merid.equals(other$merid)) {
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

            Object this$certPath = this.getCertPath();
            Object other$certPath = other.getCertPath();
            if(this$certPath == null) {
               if(other$certPath != null) {
                  return false;
               }
            } else if(!this$certPath.equals(other$certPath)) {
               return false;
            }

            Object this$certPwd = this.getCertPwd();
            Object other$certPwd = other.getCertPwd();
            if(this$certPwd == null) {
               if(other$certPwd != null) {
                  return false;
               }
            } else if(!this$certPwd.equals(other$certPwd)) {
               return false;
            }

            Object this$certType = this.getCertType();
            Object other$certType = other.getCertType();
            if(this$certType == null) {
               if(other$certType != null) {
                  return false;
               }
            } else if(!this$certType.equals(other$certType)) {
               return false;
            }

            Object this$validateCertDir = this.getValidateCertDir();
            Object other$validateCertDir = other.getValidateCertDir();
            if(this$validateCertDir == null) {
               if(other$validateCertDir != null) {
                  return false;
               }
            } else if(!this$validateCertDir.equals(other$validateCertDir)) {
               return false;
            }

            Object this$encryptCertPath = this.getEncryptCertPath();
            Object other$encryptCertPath = other.getEncryptCertPath();
            if(this$encryptCertPath == null) {
               if(other$encryptCertPath != null) {
                  return false;
               }
            } else if(!this$encryptCertPath.equals(other$encryptCertPath)) {
               return false;
            }

            Object this$singleMode = this.getSingleMode();
            Object other$singleMode = other.getSingleMode();
            if(this$singleMode == null) {
               if(other$singleMode != null) {
                  return false;
               }
            } else if(!this$singleMode.equals(other$singleMode)) {
               return false;
            }

            Object this$transFilePatch = this.getTransFilePatch();
            Object other$transFilePatch = other.getTransFilePatch();
            if(this$transFilePatch == null) {
               if(other$transFilePatch != null) {
                  return false;
               }
            } else if(!this$transFilePatch.equals(other$transFilePatch)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof UnionSettingsDto;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $merid = this.getMerid();
      result = result * 31 + ($merid == null?0:$merid.hashCode());
      Object $refundNotifyUrl = this.getRefundNotifyUrl();
      result = result * 31 + ($refundNotifyUrl == null?0:$refundNotifyUrl.hashCode());
      Object $notifyUrl = this.getNotifyUrl();
      result = result * 31 + ($notifyUrl == null?0:$notifyUrl.hashCode());
      Object $returnUrl = this.getReturnUrl();
      result = result * 31 + ($returnUrl == null?0:$returnUrl.hashCode());
      Object $certPath = this.getCertPath();
      result = result * 31 + ($certPath == null?0:$certPath.hashCode());
      Object $certPwd = this.getCertPwd();
      result = result * 31 + ($certPwd == null?0:$certPwd.hashCode());
      Object $certType = this.getCertType();
      result = result * 31 + ($certType == null?0:$certType.hashCode());
      Object $validateCertDir = this.getValidateCertDir();
      result = result * 31 + ($validateCertDir == null?0:$validateCertDir.hashCode());
      Object $encryptCertPath = this.getEncryptCertPath();
      result = result * 31 + ($encryptCertPath == null?0:$encryptCertPath.hashCode());
      Object $singleMode = this.getSingleMode();
      result = result * 31 + ($singleMode == null?0:$singleMode.hashCode());
      Object $transFilePatch = this.getTransFilePatch();
      result = result * 31 + ($transFilePatch == null?0:$transFilePatch.hashCode());
      return result;
   }

   public String toString() {
      return "UnionSettingsDto(merid=" + this.getMerid() + ", refundNotifyUrl=" + this.getRefundNotifyUrl() + ", notifyUrl=" + this.getNotifyUrl() + ", returnUrl=" + this.getReturnUrl() + ", certPath=" + this.getCertPath() + ", certPwd=" + this.getCertPwd() + ", certType=" + this.getCertType() + ", validateCertDir=" + this.getValidateCertDir() + ", encryptCertPath=" + this.getEncryptCertPath() + ", singleMode=" + this.getSingleMode() + ", transFilePatch=" + this.getTransFilePatch() + ")";
   }
}
