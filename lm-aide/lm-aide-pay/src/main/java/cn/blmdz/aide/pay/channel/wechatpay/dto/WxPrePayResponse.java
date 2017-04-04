package io.terminus.lib.pay.channel.wechatpay.dto;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.thoughtworks.xstream.annotations.XStreamAlias;

@XStreamAlias("xml")
public class WxPrePayResponse {
   @XStreamAlias("appid")
   private String appid;
   @XStreamAlias("mch_id")
   private String mchId;
   @XStreamAlias("sub_mch_id")
   private String subMchId;
   @XStreamAlias("device_info")
   private String deviceInfo;
   @XStreamAlias("nonce_str")
   private String nonceStr;
   @XStreamAlias("sign")
   private String sign;
   @XStreamAlias("trade_type")
   private String tradeType;
   @XStreamAlias("prepay_id")
   private String prepayId;
   @XStreamAlias("code_url")
   private String codeUrl;
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
      return Objects.equal(WxPrePayResponse.Code.SUCCESS, WxPrePayResponse.Code.from(this.resultCode)) && Objects.equal(WxPrePayResponse.Code.SUCCESS, WxPrePayResponse.Code.from(this.returnCode));
   }

   public String getErrorMsg() {
      return Strings.isNullOrEmpty(this.returnMsg)?this.errCodeDes:this.returnMsg;
   }

   public String getAppid() {
      return this.appid;
   }

   public void setAppid(String appid) {
      this.appid = appid;
   }

   public String getMchId() {
      return this.mchId;
   }

   public void setMchId(String mchId) {
      this.mchId = mchId;
   }

   public String getSubMchId() {
      return this.subMchId;
   }

   public void setSubMchId(String subMchId) {
      this.subMchId = subMchId;
   }

   public String getDeviceInfo() {
      return this.deviceInfo;
   }

   public void setDeviceInfo(String deviceInfo) {
      this.deviceInfo = deviceInfo;
   }

   public String getNonceStr() {
      return this.nonceStr;
   }

   public void setNonceStr(String nonceStr) {
      this.nonceStr = nonceStr;
   }

   public String getSign() {
      return this.sign;
   }

   public void setSign(String sign) {
      this.sign = sign;
   }

   public String getTradeType() {
      return this.tradeType;
   }

   public void setTradeType(String tradeType) {
      this.tradeType = tradeType;
   }

   public String getPrepayId() {
      return this.prepayId;
   }

   public void setPrepayId(String prepayId) {
      this.prepayId = prepayId;
   }

   public String getCodeUrl() {
      return this.codeUrl;
   }

   public void setCodeUrl(String codeUrl) {
      this.codeUrl = codeUrl;
   }

   public static enum Code {
      SUCCESS("SUCCESS"),
      FAIL("FAIL");

      private String value;

      private Code(String value) {
         this.value = value;
      }

      public static WxPrePayResponse.Code from(String value) {
         for(WxPrePayResponse.Code code : values()) {
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
