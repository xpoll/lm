package io.terminus.lib.pay.channel.wechatpay.dto;

import java.util.List;

public class WxPayBillResponse {
   private String totalCount;
   private String totalFee;
   private String totalRefundFee;
   private String totalCouponRefundFee;
   private String totalPoundageFee;
   private String sign;
   private List wxPayBillDtos;

   public static WxPayBillResponse newInstance(List cols) {
      WxPayBillResponse response = new WxPayBillResponse();
      response.setTotalCount((String)cols.get(0));
      response.setTotalFee((String)cols.get(1));
      response.setTotalRefundFee((String)cols.get(2));
      response.setTotalCouponRefundFee((String)cols.get(3));
      response.setTotalPoundageFee((String)cols.get(4));
      return response;
   }

   public String toString() {
      return "WxPayBillResponse(totalCount=" + this.getTotalCount() + ", totalFee=" + this.getTotalFee() + ", totalRefundFee=" + this.getTotalRefundFee() + ", totalCouponRefundFee=" + this.getTotalCouponRefundFee() + ", totalPoundageFee=" + this.getTotalPoundageFee() + ", sign=" + this.getSign() + ", wxPayBillDtos=" + this.getWxPayBillDtos() + ")";
   }

   public String getTotalCount() {
      return this.totalCount;
   }

   public void setTotalCount(String totalCount) {
      this.totalCount = totalCount;
   }

   public String getTotalFee() {
      return this.totalFee;
   }

   public void setTotalFee(String totalFee) {
      this.totalFee = totalFee;
   }

   public String getTotalRefundFee() {
      return this.totalRefundFee;
   }

   public void setTotalRefundFee(String totalRefundFee) {
      this.totalRefundFee = totalRefundFee;
   }

   public String getTotalCouponRefundFee() {
      return this.totalCouponRefundFee;
   }

   public void setTotalCouponRefundFee(String totalCouponRefundFee) {
      this.totalCouponRefundFee = totalCouponRefundFee;
   }

   public String getTotalPoundageFee() {
      return this.totalPoundageFee;
   }

   public void setTotalPoundageFee(String totalPoundageFee) {
      this.totalPoundageFee = totalPoundageFee;
   }

   public String getSign() {
      return this.sign;
   }

   public void setSign(String sign) {
      this.sign = sign;
   }

   public List getWxPayBillDtos() {
      return this.wxPayBillDtos;
   }

   public void setWxPayBillDtos(List wxPayBillDtos) {
      this.wxPayBillDtos = wxPayBillDtos;
   }
}
