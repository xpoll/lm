package io.terminus.lib.pay.channel.wechatpay.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.thoughtworks.xstream.annotations.XStreamAlias;

@XStreamAlias("xml")
public class WxPayNotifyDto {
   @XStreamAlias("return_code")
   private String returnCode;
   @XStreamAlias("return_msg")
   private String returnMsg;
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
   @XStreamAlias("result_code")
   private String resultCode;
   @XStreamAlias("err_code")
   private String errCode;
   @XStreamAlias("err_code_des")
   private String errCodeDes;
   @XStreamAlias("openid")
   private String openid;
   @XStreamAlias("is_subscribe")
   private String isSubscribe;
   @XStreamAlias("bank_type")
   private String bankType;
   @XStreamAlias("total_fee")
   private Integer totalFee;
   @XStreamAlias("coupon_fee")
   private Integer couponFee;
   @XStreamAlias("fee_type")
   private String feeType;
   @XStreamAlias("transaction_id")
   private String transactionId;
   @XStreamAlias("out_trade_no")
   private String outTradeNo;
   @XStreamAlias("attach")
   private String attach;
   @XStreamAlias("trade_type")
   private String tradeType;
   @XStreamAlias("prepay_id")
   private String prepayId;
   @XStreamAlias("code_url")
   private String codeUrl;
   @XStreamAlias("time_end")
   private String timeEnd;

   @JsonIgnore
   public boolean isSuccess() {
      return Objects.equal(WxPayNotifyDto.Code.SUCCESS, WxPayNotifyDto.Code.from(this.resultCode)) && Objects.equal(WxPayNotifyDto.Code.SUCCESS, WxPayNotifyDto.Code.from(this.returnCode));
   }

   @JsonIgnore
   public String getErrorMsg() {
      return Strings.isNullOrEmpty(this.returnMsg)?this.errCodeDes:this.returnMsg;
   }

   public String toString() {
      return "WxPayNotifyDto(returnCode=" + this.getReturnCode() + ", returnMsg=" + this.getReturnMsg() + ", appid=" + this.getAppid() + ", mchId=" + this.getMchId() + ", subMchId=" + this.getSubMchId() + ", deviceInfo=" + this.getDeviceInfo() + ", nonceStr=" + this.getNonceStr() + ", sign=" + this.getSign() + ", resultCode=" + this.getResultCode() + ", errCode=" + this.getErrCode() + ", errCodeDes=" + this.getErrCodeDes() + ", openid=" + this.getOpenid() + ", isSubscribe=" + this.getIsSubscribe() + ", bankType=" + this.getBankType() + ", totalFee=" + this.getTotalFee() + ", couponFee=" + this.getCouponFee() + ", feeType=" + this.getFeeType() + ", transactionId=" + this.getTransactionId() + ", outTradeNo=" + this.getOutTradeNo() + ", attach=" + this.getAttach() + ", tradeType=" + this.getTradeType() + ", prepayId=" + this.getPrepayId() + ", codeUrl=" + this.getCodeUrl() + ", timeEnd=" + this.getTimeEnd() + ")";
   }

   public String getReturnCode() {
      return this.returnCode;
   }

   public void setReturnCode(String returnCode) {
      this.returnCode = returnCode;
   }

   public String getReturnMsg() {
      return this.returnMsg;
   }

   public void setReturnMsg(String returnMsg) {
      this.returnMsg = returnMsg;
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

   public String getResultCode() {
      return this.resultCode;
   }

   public void setResultCode(String resultCode) {
      this.resultCode = resultCode;
   }

   public String getErrCode() {
      return this.errCode;
   }

   public void setErrCode(String errCode) {
      this.errCode = errCode;
   }

   public String getErrCodeDes() {
      return this.errCodeDes;
   }

   public void setErrCodeDes(String errCodeDes) {
      this.errCodeDes = errCodeDes;
   }

   public String getOpenid() {
      return this.openid;
   }

   public void setOpenid(String openid) {
      this.openid = openid;
   }

   public String getIsSubscribe() {
      return this.isSubscribe;
   }

   public void setIsSubscribe(String isSubscribe) {
      this.isSubscribe = isSubscribe;
   }

   public String getBankType() {
      return this.bankType;
   }

   public void setBankType(String bankType) {
      this.bankType = bankType;
   }

   public Integer getTotalFee() {
      return this.totalFee;
   }

   public void setTotalFee(Integer totalFee) {
      this.totalFee = totalFee;
   }

   public Integer getCouponFee() {
      return this.couponFee;
   }

   public void setCouponFee(Integer couponFee) {
      this.couponFee = couponFee;
   }

   public String getFeeType() {
      return this.feeType;
   }

   public void setFeeType(String feeType) {
      this.feeType = feeType;
   }

   public String getTransactionId() {
      return this.transactionId;
   }

   public void setTransactionId(String transactionId) {
      this.transactionId = transactionId;
   }

   public String getOutTradeNo() {
      return this.outTradeNo;
   }

   public void setOutTradeNo(String outTradeNo) {
      this.outTradeNo = outTradeNo;
   }

   public String getAttach() {
      return this.attach;
   }

   public void setAttach(String attach) {
      this.attach = attach;
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

   public String getTimeEnd() {
      return this.timeEnd;
   }

   public void setTimeEnd(String timeEnd) {
      this.timeEnd = timeEnd;
   }

   public static enum Code {
      SUCCESS("SUCCESS"),
      FAIL("FAIL");

      private String value;

      private Code(String value) {
         this.value = value;
      }

      public static WxPayNotifyDto.Code from(String value) {
         for(WxPayNotifyDto.Code code : values()) {
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

   public static enum ErrorCode {
      NOAUTH("NOAUTH", "商户无权限"),
      NOTENOUGH("NOTENOUGH", "余额不足"),
      NOTSUPORTCARD("NOTSUPORTCARD", "不支持卡类型"),
      ORDERPAID("ORDERPAID", "商户订单已支付"),
      ORDERCLOSED("ORDERCLOSED", "订单已关闭"),
      BANKERROR("BANKERROR", "银行系统异常"),
      SYSTEMERROR("SYSTEMERROR", "系统错误错");

      private String value;
      private String display;

      private ErrorCode(String value, String display) {
         this.value = value;
         this.display = display;
      }

      public static WxPayNotifyDto.ErrorCode from(String value) {
         for(WxPayNotifyDto.ErrorCode code : values()) {
            if(Objects.equal(value, code.value)) {
               return code;
            }
         }

         return null;
      }

      public String getValue() {
         return this.value;
      }

      public String getDisplay() {
         return this.display;
      }
   }
}
