package io.terminus.lib.pay.channel.wechatpay.dto;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.thoughtworks.xstream.annotations.XStreamAlias;

@XStreamAlias("xml")
public class WxSyncResponse {
   @XStreamAlias("return_code")
   private String returnCode;
   @XStreamAlias("return_msg")
   private String returnMsg;
   @XStreamAlias("result_code")
   private String resultCode;
   @XStreamAlias("err_code")
   private String errCode;
   @XStreamAlias("err_code_des")
   private String errCodeDes;

   public boolean isSuccess() {
      return Objects.equal(WxSyncResponse.Code.SUCCESS, WxSyncResponse.Code.from(this.resultCode)) && Objects.equal(WxSyncResponse.Code.SUCCESS, WxSyncResponse.Code.from(this.returnCode));
   }

   public String getErrorMsg() {
      return Strings.isNullOrEmpty(this.returnMsg)?this.errCodeDes:this.returnMsg;
   }

   public String getReturnCode() {
      return this.returnCode;
   }

   public String getReturnMsg() {
      return this.returnMsg;
   }

   public String getResultCode() {
      return this.resultCode;
   }

   public String getErrCode() {
      return this.errCode;
   }

   public String getErrCodeDes() {
      return this.errCodeDes;
   }

   public void setReturnCode(String returnCode) {
      this.returnCode = returnCode;
   }

   public void setReturnMsg(String returnMsg) {
      this.returnMsg = returnMsg;
   }

   public void setResultCode(String resultCode) {
      this.resultCode = resultCode;
   }

   public void setErrCode(String errCode) {
      this.errCode = errCode;
   }

   public void setErrCodeDes(String errCodeDes) {
      this.errCodeDes = errCodeDes;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof WxSyncResponse)) {
         return false;
      } else {
         WxSyncResponse other = (WxSyncResponse)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$returnCode = this.getReturnCode();
            Object other$returnCode = other.getReturnCode();
            if(this$returnCode == null) {
               if(other$returnCode != null) {
                  return false;
               }
            } else if(!this$returnCode.equals(other$returnCode)) {
               return false;
            }

            Object this$returnMsg = this.getReturnMsg();
            Object other$returnMsg = other.getReturnMsg();
            if(this$returnMsg == null) {
               if(other$returnMsg != null) {
                  return false;
               }
            } else if(!this$returnMsg.equals(other$returnMsg)) {
               return false;
            }

            Object this$resultCode = this.getResultCode();
            Object other$resultCode = other.getResultCode();
            if(this$resultCode == null) {
               if(other$resultCode != null) {
                  return false;
               }
            } else if(!this$resultCode.equals(other$resultCode)) {
               return false;
            }

            Object this$errCode = this.getErrCode();
            Object other$errCode = other.getErrCode();
            if(this$errCode == null) {
               if(other$errCode != null) {
                  return false;
               }
            } else if(!this$errCode.equals(other$errCode)) {
               return false;
            }

            Object this$errCodeDes = this.getErrCodeDes();
            Object other$errCodeDes = other.getErrCodeDes();
            if(this$errCodeDes == null) {
               if(other$errCodeDes != null) {
                  return false;
               }
            } else if(!this$errCodeDes.equals(other$errCodeDes)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof WxSyncResponse;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $returnCode = this.getReturnCode();
      result = result * 59 + ($returnCode == null?0:$returnCode.hashCode());
      Object $returnMsg = this.getReturnMsg();
      result = result * 59 + ($returnMsg == null?0:$returnMsg.hashCode());
      Object $resultCode = this.getResultCode();
      result = result * 59 + ($resultCode == null?0:$resultCode.hashCode());
      Object $errCode = this.getErrCode();
      result = result * 59 + ($errCode == null?0:$errCode.hashCode());
      Object $errCodeDes = this.getErrCodeDes();
      result = result * 59 + ($errCodeDes == null?0:$errCodeDes.hashCode());
      return result;
   }

   public String toString() {
      return "WxSyncResponse(returnCode=" + this.getReturnCode() + ", returnMsg=" + this.getReturnMsg() + ", resultCode=" + this.getResultCode() + ", errCode=" + this.getErrCode() + ", errCodeDes=" + this.getErrCodeDes() + ")";
   }

   public static enum Code {
      SUCCESS("SUCCESS"),
      FAIL("FAIL");

      private String value;

      private Code(String value) {
         this.value = value;
      }

      public static WxSyncResponse.Code from(String value) {
         for(WxSyncResponse.Code code : values()) {
            if(Objects.equal(value, code.value)) {
               return code;
            }
         }

         return null;
      }

      public String getValue() {
         return this.value;
      }
   }
}
