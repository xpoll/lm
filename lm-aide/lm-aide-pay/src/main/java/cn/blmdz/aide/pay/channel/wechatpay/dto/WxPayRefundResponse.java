package io.terminus.lib.pay.channel.wechatpay.dto;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.thoughtworks.xstream.annotations.XStreamAlias;

@XStreamAlias("xml")
public class WxPayRefundResponse {
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
   @XStreamAlias("transaction_id")
   private String transactionId;
   @XStreamAlias("out_trade_no")
   private String outTradeNo;
   @XStreamAlias("out_refund_no")
   private String outRefundNo;
   @XStreamAlias("refund_id")
   private String refundId;
   @XStreamAlias("refund_channel")
   private String refundChannel;
   @XStreamAlias("refund_fee")
   private Integer refundFee;
   @XStreamAlias("coupon_refund_fee")
   private Integer couponRefundFee;

   public boolean isSuccess() {
      return Objects.equal(WxPayRefundResponse.Code.SUCCESS, WxPayRefundResponse.Code.from(this.resultCode)) && Objects.equal(WxPayRefundResponse.Code.SUCCESS, WxPayRefundResponse.Code.from(this.returnCode));
   }

   public String getErrorMsg() {
      return Strings.isNullOrEmpty(this.returnMsg)?this.errCodeDes:this.returnMsg;
   }

   public String toString() {
      return "WxPayRefundResponse(returnCode=" + this.getReturnCode() + ", returnMsg=" + this.getReturnMsg() + ", resultCode=" + this.getResultCode() + ", errCode=" + this.getErrCode() + ", errCodeDes=" + this.getErrCodeDes() + ", appid=" + this.getAppid() + ", mchId=" + this.getMchId() + ", subMchId=" + this.getSubMchId() + ", deviceInfo=" + this.getDeviceInfo() + ", nonceStr=" + this.getNonceStr() + ", sign=" + this.getSign() + ", transactionId=" + this.getTransactionId() + ", outTradeNo=" + this.getOutTradeNo() + ", outRefundNo=" + this.getOutRefundNo() + ", refundId=" + this.getRefundId() + ", refundChannel=" + this.getRefundChannel() + ", refundFee=" + this.getRefundFee() + ", couponRefundFee=" + this.getCouponRefundFee() + ")";
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

   public String getOutRefundNo() {
      return this.outRefundNo;
   }

   public void setOutRefundNo(String outRefundNo) {
      this.outRefundNo = outRefundNo;
   }

   public String getRefundId() {
      return this.refundId;
   }

   public void setRefundId(String refundId) {
      this.refundId = refundId;
   }

   public String getRefundChannel() {
      return this.refundChannel;
   }

   public void setRefundChannel(String refundChannel) {
      this.refundChannel = refundChannel;
   }

   public Integer getRefundFee() {
      return this.refundFee;
   }

   public void setRefundFee(Integer refundFee) {
      this.refundFee = refundFee;
   }

   public Integer getCouponRefundFee() {
      return this.couponRefundFee;
   }

   public void setCouponRefundFee(Integer couponRefundFee) {
      this.couponRefundFee = couponRefundFee;
   }

   public static enum Code {
      SUCCESS("SUCCESS"),
      FAIL("FAIL");

      private String value;

      private Code(String value) {
         this.value = value;
      }

      public static WxPayRefundResponse.Code from(String value) {
         for(WxPayRefundResponse.Code code : values()) {
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
