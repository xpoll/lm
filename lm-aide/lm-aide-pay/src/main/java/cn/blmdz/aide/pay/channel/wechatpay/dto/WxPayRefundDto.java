package io.terminus.lib.pay.channel.wechatpay.dto;

public class WxPayRefundDto {
   private Integer id;
   private String outRefundNo;
   private String refundId;
   private String refundChannel;
   private String refundFee;
   private String couponFefundFee;
   private String refundStatus;

   public String toString() {
      return "WxPayRefundDto(id=" + this.getId() + ", outRefundNo=" + this.getOutRefundNo() + ", refundId=" + this.getRefundId() + ", refundChannel=" + this.getRefundChannel() + ", refundFee=" + this.getRefundFee() + ", couponFefundFee=" + this.getCouponFefundFee() + ", refundStatus=" + this.getRefundStatus() + ")";
   }

   public Integer getId() {
      return this.id;
   }

   public void setId(Integer id) {
      this.id = id;
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

   public String getRefundFee() {
      return this.refundFee;
   }

   public void setRefundFee(String refundFee) {
      this.refundFee = refundFee;
   }

   public String getCouponFefundFee() {
      return this.couponFefundFee;
   }

   public void setCouponFefundFee(String couponFefundFee) {
      this.couponFefundFee = couponFefundFee;
   }

   public String getRefundStatus() {
      return this.refundStatus;
   }

   public void setRefundStatus(String refundStatus) {
      this.refundStatus = refundStatus;
   }
}
