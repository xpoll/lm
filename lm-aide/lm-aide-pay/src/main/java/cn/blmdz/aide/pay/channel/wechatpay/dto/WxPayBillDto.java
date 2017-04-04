package io.terminus.lib.pay.channel.wechatpay.dto;

import com.google.common.base.Strings;
import java.util.Date;
import java.util.List;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class WxPayBillDto {
   private static final DateTimeFormatter DFT = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");
   private Date tradeTime;
   private String appid;
   private String mchId;
   private String subMchId;
   private String deviceInfo;
   private String transactionId;
   private String outTradeNo;
   private String openId;
   private String tradeType;
   private String tradeStatus;
   private String bankType;
   private String feeType;
   private String totalFee;
   private String couponFee;
   private Date refundApplyDate;
   private Date refundSuccessDate;
   private String refundId;
   private String outRefundNo;
   private String refundFee;
   private String couponRefundFee;
   private String refundChannel;
   private String refundStatus;
   private String body;
   private String attach;
   private String poundageFee;
   private String rate;
   private String bankOrderNo;
   private String tradeInfo;

   public static WxPayBillDto all(List cols) {
      WxPayBillDto dto = new WxPayBillDto();
      if(!Strings.isNullOrEmpty((String)cols.get(0))) {
         dto.setTradeTime(DFT.parseDateTime((String)cols.get(0)).toDate());
      }

      dto.setAppid((String)cols.get(1));
      dto.setMchId((String)cols.get(2));
      dto.setSubMchId((String)cols.get(3));
      dto.setDeviceInfo((String)cols.get(4));
      dto.setTransactionId((String)cols.get(5));
      dto.setOutTradeNo((String)cols.get(6));
      dto.setOpenId((String)cols.get(7));
      dto.setTradeType((String)cols.get(8));
      dto.setTradeStatus((String)cols.get(9));
      dto.setBankType((String)cols.get(10));
      dto.setFeeType((String)cols.get(11));
      dto.setTotalFee((String)cols.get(12));
      dto.setCouponFee((String)cols.get(13));
      dto.setRefundId((String)cols.get(14));
      dto.setOutRefundNo((String)cols.get(15));
      dto.setRefundFee((String)cols.get(16));
      dto.setCouponRefundFee((String)cols.get(17));
      dto.setRefundChannel((String)cols.get(18));
      dto.setRefundStatus((String)cols.get(19));
      dto.setBody((String)cols.get(20));
      dto.setAttach((String)cols.get(21));
      dto.setPoundageFee((String)cols.get(22));
      dto.setRate((String)cols.get(23));
      return dto;
   }

   public static WxPayBillDto success(List cols) {
      WxPayBillDto dto = new WxPayBillDto();
      if(!Strings.isNullOrEmpty((String)cols.get(0))) {
         dto.setTradeTime(DFT.parseDateTime((String)cols.get(0)).toDate());
      }

      dto.setAppid((String)cols.get(1));
      dto.setMchId((String)cols.get(2));
      dto.setSubMchId((String)cols.get(3));
      dto.setDeviceInfo((String)cols.get(4));
      dto.setTransactionId((String)cols.get(5));
      dto.setOutTradeNo((String)cols.get(6));
      dto.setOpenId((String)cols.get(7));
      dto.setTradeType((String)cols.get(8));
      dto.setTradeStatus((String)cols.get(9));
      dto.setBankType((String)cols.get(10));
      dto.setFeeType((String)cols.get(11));
      dto.setTotalFee((String)cols.get(12));
      dto.setCouponFee((String)cols.get(13));
      dto.setBody((String)cols.get(14));
      dto.setAttach((String)cols.get(15));
      dto.setPoundageFee((String)cols.get(16));
      dto.setRate((String)cols.get(17));
      return dto;
   }

   public static WxPayBillDto refund(List cols) {
      WxPayBillDto dto = new WxPayBillDto();
      if(!Strings.isNullOrEmpty((String)cols.get(0))) {
         dto.setTradeTime(DFT.parseDateTime((String)cols.get(0)).toDate());
      }

      dto.setAppid((String)cols.get(1));
      dto.setMchId((String)cols.get(2));
      dto.setSubMchId((String)cols.get(3));
      dto.setDeviceInfo((String)cols.get(4));
      dto.setTransactionId((String)cols.get(5));
      dto.setOutTradeNo((String)cols.get(6));
      dto.setOpenId((String)cols.get(7));
      dto.setTradeType((String)cols.get(8));
      dto.setTradeStatus((String)cols.get(9));
      dto.setBankType((String)cols.get(10));
      dto.setFeeType((String)cols.get(11));
      dto.setTotalFee((String)cols.get(12));
      dto.setCouponFee((String)cols.get(13));
      if(!Strings.isNullOrEmpty((String)cols.get(14))) {
         dto.setRefundApplyDate(DFT.parseDateTime((String)cols.get(14)).toDate());
      }

      if(!Strings.isNullOrEmpty((String)cols.get(15))) {
         dto.setRefundSuccessDate(DFT.parseDateTime((String)cols.get(15)).toDate());
      }

      dto.setRefundId((String)cols.get(16));
      dto.setOutRefundNo((String)cols.get(17));
      dto.setRefundFee((String)cols.get(18));
      dto.setCouponRefundFee((String)cols.get(19));
      dto.setRefundChannel((String)cols.get(20));
      dto.setRefundStatus((String)cols.get(21));
      dto.setBody((String)cols.get(22));
      dto.setAttach((String)cols.get(23));
      dto.setPoundageFee((String)cols.get(24));
      dto.setRate((String)cols.get(25));
      return dto;
   }

   public String toString() {
      return "WxPayBillDto(tradeTime=" + this.getTradeTime() + ", appid=" + this.getAppid() + ", mchId=" + this.getMchId() + ", subMchId=" + this.getSubMchId() + ", deviceInfo=" + this.getDeviceInfo() + ", transactionId=" + this.getTransactionId() + ", outTradeNo=" + this.getOutTradeNo() + ", openId=" + this.getOpenId() + ", tradeType=" + this.getTradeType() + ", tradeStatus=" + this.getTradeStatus() + ", bankType=" + this.getBankType() + ", feeType=" + this.getFeeType() + ", totalFee=" + this.getTotalFee() + ", couponFee=" + this.getCouponFee() + ", refundApplyDate=" + this.getRefundApplyDate() + ", refundSuccessDate=" + this.getRefundSuccessDate() + ", refundId=" + this.getRefundId() + ", outRefundNo=" + this.getOutRefundNo() + ", refundFee=" + this.getRefundFee() + ", couponRefundFee=" + this.getCouponRefundFee() + ", refundChannel=" + this.getRefundChannel() + ", refundStatus=" + this.getRefundStatus() + ", body=" + this.getBody() + ", attach=" + this.getAttach() + ", poundageFee=" + this.getPoundageFee() + ", rate=" + this.getRate() + ", bankOrderNo=" + this.getBankOrderNo() + ", tradeInfo=" + this.getTradeInfo() + ")";
   }

   public Date getTradeTime() {
      return this.tradeTime;
   }

   public void setTradeTime(Date tradeTime) {
      this.tradeTime = tradeTime;
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

   public String getOpenId() {
      return this.openId;
   }

   public void setOpenId(String openId) {
      this.openId = openId;
   }

   public String getTradeType() {
      return this.tradeType;
   }

   public void setTradeType(String tradeType) {
      this.tradeType = tradeType;
   }

   public String getTradeStatus() {
      return this.tradeStatus;
   }

   public void setTradeStatus(String tradeStatus) {
      this.tradeStatus = tradeStatus;
   }

   public String getBankType() {
      return this.bankType;
   }

   public void setBankType(String bankType) {
      this.bankType = bankType;
   }

   public String getFeeType() {
      return this.feeType;
   }

   public void setFeeType(String feeType) {
      this.feeType = feeType;
   }

   public String getTotalFee() {
      return this.totalFee;
   }

   public void setTotalFee(String totalFee) {
      this.totalFee = totalFee;
   }

   public String getCouponFee() {
      return this.couponFee;
   }

   public void setCouponFee(String couponFee) {
      this.couponFee = couponFee;
   }

   public Date getRefundApplyDate() {
      return this.refundApplyDate;
   }

   public void setRefundApplyDate(Date refundApplyDate) {
      this.refundApplyDate = refundApplyDate;
   }

   public Date getRefundSuccessDate() {
      return this.refundSuccessDate;
   }

   public void setRefundSuccessDate(Date refundSuccessDate) {
      this.refundSuccessDate = refundSuccessDate;
   }

   public String getRefundId() {
      return this.refundId;
   }

   public void setRefundId(String refundId) {
      this.refundId = refundId;
   }

   public String getOutRefundNo() {
      return this.outRefundNo;
   }

   public void setOutRefundNo(String outRefundNo) {
      this.outRefundNo = outRefundNo;
   }

   public String getRefundFee() {
      return this.refundFee;
   }

   public void setRefundFee(String refundFee) {
      this.refundFee = refundFee;
   }

   public String getCouponRefundFee() {
      return this.couponRefundFee;
   }

   public void setCouponRefundFee(String couponRefundFee) {
      this.couponRefundFee = couponRefundFee;
   }

   public String getRefundChannel() {
      return this.refundChannel;
   }

   public void setRefundChannel(String refundChannel) {
      this.refundChannel = refundChannel;
   }

   public String getRefundStatus() {
      return this.refundStatus;
   }

   public void setRefundStatus(String refundStatus) {
      this.refundStatus = refundStatus;
   }

   public String getBody() {
      return this.body;
   }

   public void setBody(String body) {
      this.body = body;
   }

   public String getAttach() {
      return this.attach;
   }

   public void setAttach(String attach) {
      this.attach = attach;
   }

   public String getPoundageFee() {
      return this.poundageFee;
   }

   public void setPoundageFee(String poundageFee) {
      this.poundageFee = poundageFee;
   }

   public String getRate() {
      return this.rate;
   }

   public void setRate(String rate) {
      this.rate = rate;
   }

   public String getBankOrderNo() {
      return this.bankOrderNo;
   }

   public void setBankOrderNo(String bankOrderNo) {
      this.bankOrderNo = bankOrderNo;
   }

   public String getTradeInfo() {
      return this.tradeInfo;
   }

   public void setTradeInfo(String tradeInfo) {
      this.tradeInfo = tradeInfo;
   }
}
