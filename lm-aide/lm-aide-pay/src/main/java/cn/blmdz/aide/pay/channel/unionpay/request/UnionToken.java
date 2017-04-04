package io.terminus.lib.pay.channel.unionpay.request;

import io.terminus.lib.pay.common.Token;
import java.beans.ConstructorProperties;

public class UnionToken implements Token {
   private String merId;
   private String frontTransUrl;
   private String backTransUrl;
   private String singleQueryUrl;
   private String batchTransUrl;
   private String fileTransUrl;

   public String getMerId() {
      return this.merId;
   }

   public String getFrontTransUrl() {
      return this.frontTransUrl;
   }

   public String getBackTransUrl() {
      return this.backTransUrl;
   }

   public String getSingleQueryUrl() {
      return this.singleQueryUrl;
   }

   public String getBatchTransUrl() {
      return this.batchTransUrl;
   }

   public String getFileTransUrl() {
      return this.fileTransUrl;
   }

   public void setMerId(String merId) {
      this.merId = merId;
   }

   public void setFrontTransUrl(String frontTransUrl) {
      this.frontTransUrl = frontTransUrl;
   }

   public void setBackTransUrl(String backTransUrl) {
      this.backTransUrl = backTransUrl;
   }

   public void setSingleQueryUrl(String singleQueryUrl) {
      this.singleQueryUrl = singleQueryUrl;
   }

   public void setBatchTransUrl(String batchTransUrl) {
      this.batchTransUrl = batchTransUrl;
   }

   public void setFileTransUrl(String fileTransUrl) {
      this.fileTransUrl = fileTransUrl;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof UnionToken)) {
         return false;
      } else {
         UnionToken other = (UnionToken)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$merId = this.getMerId();
            Object other$merId = other.getMerId();
            if(this$merId == null) {
               if(other$merId != null) {
                  return false;
               }
            } else if(!this$merId.equals(other$merId)) {
               return false;
            }

            Object this$frontTransUrl = this.getFrontTransUrl();
            Object other$frontTransUrl = other.getFrontTransUrl();
            if(this$frontTransUrl == null) {
               if(other$frontTransUrl != null) {
                  return false;
               }
            } else if(!this$frontTransUrl.equals(other$frontTransUrl)) {
               return false;
            }

            Object this$backTransUrl = this.getBackTransUrl();
            Object other$backTransUrl = other.getBackTransUrl();
            if(this$backTransUrl == null) {
               if(other$backTransUrl != null) {
                  return false;
               }
            } else if(!this$backTransUrl.equals(other$backTransUrl)) {
               return false;
            }

            Object this$singleQueryUrl = this.getSingleQueryUrl();
            Object other$singleQueryUrl = other.getSingleQueryUrl();
            if(this$singleQueryUrl == null) {
               if(other$singleQueryUrl != null) {
                  return false;
               }
            } else if(!this$singleQueryUrl.equals(other$singleQueryUrl)) {
               return false;
            }

            Object this$batchTransUrl = this.getBatchTransUrl();
            Object other$batchTransUrl = other.getBatchTransUrl();
            if(this$batchTransUrl == null) {
               if(other$batchTransUrl != null) {
                  return false;
               }
            } else if(!this$batchTransUrl.equals(other$batchTransUrl)) {
               return false;
            }

            Object this$fileTransUrl = this.getFileTransUrl();
            Object other$fileTransUrl = other.getFileTransUrl();
            if(this$fileTransUrl == null) {
               if(other$fileTransUrl != null) {
                  return false;
               }
            } else if(!this$fileTransUrl.equals(other$fileTransUrl)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof UnionToken;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $merId = this.getMerId();
      result = result * 59 + ($merId == null?0:$merId.hashCode());
      Object $frontTransUrl = this.getFrontTransUrl();
      result = result * 59 + ($frontTransUrl == null?0:$frontTransUrl.hashCode());
      Object $backTransUrl = this.getBackTransUrl();
      result = result * 59 + ($backTransUrl == null?0:$backTransUrl.hashCode());
      Object $singleQueryUrl = this.getSingleQueryUrl();
      result = result * 59 + ($singleQueryUrl == null?0:$singleQueryUrl.hashCode());
      Object $batchTransUrl = this.getBatchTransUrl();
      result = result * 59 + ($batchTransUrl == null?0:$batchTransUrl.hashCode());
      Object $fileTransUrl = this.getFileTransUrl();
      result = result * 59 + ($fileTransUrl == null?0:$fileTransUrl.hashCode());
      return result;
   }

   public String toString() {
      return "UnionToken(merId=" + this.getMerId() + ", frontTransUrl=" + this.getFrontTransUrl() + ", backTransUrl=" + this.getBackTransUrl() + ", singleQueryUrl=" + this.getSingleQueryUrl() + ", batchTransUrl=" + this.getBatchTransUrl() + ", fileTransUrl=" + this.getFileTransUrl() + ")";
   }

   public UnionToken() {
   }

   @ConstructorProperties({"merId", "frontTransUrl", "backTransUrl", "singleQueryUrl", "batchTransUrl", "fileTransUrl"})
   public UnionToken(String merId, String frontTransUrl, String backTransUrl, String singleQueryUrl, String batchTransUrl, String fileTransUrl) {
      this.merId = merId;
      this.frontTransUrl = frontTransUrl;
      this.backTransUrl = backTransUrl;
      this.singleQueryUrl = singleQueryUrl;
      this.batchTransUrl = batchTransUrl;
      this.fileTransUrl = fileTransUrl;
   }
}
