package io.terminus.parana.pay.mock.model;

import java.io.Serializable;
import java.util.Date;

public class MockWechatpayTrans implements Serializable {
   private static final long serialVersionUID = -8020774200815852729L;
   private Long id;
   private String transactionId;
   private String outTradeNo;
   private String tradeStatus;
   private String tradeTime;
   private String appid;
   private String mchId;
   private String subMchId;
   private String deviceInfo;
   private String openId;
   private String tradeType;
   private String bankType;
   private String feeType;
   private String totalFee;
   private String couponFee;
   private String refundApplyDate;
   private String refundSuccessDate;
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
   private Date tradeAt;

   public Long getId() {
      return this.id;
   }

   public String getTransactionId() {
      return this.transactionId;
   }

   public String getOutTradeNo() {
      return this.outTradeNo;
   }

   public String getTradeStatus() {
      return this.tradeStatus;
   }

   public String getTradeTime() {
      return this.tradeTime;
   }

   public String getAppid() {
      return this.appid;
   }

   public String getMchId() {
      return this.mchId;
   }

   public String getSubMchId() {
      return this.subMchId;
   }

   public String getDeviceInfo() {
      return this.deviceInfo;
   }

   public String getOpenId() {
      return this.openId;
   }

   public String getTradeType() {
      return this.tradeType;
   }

   public String getBankType() {
      return this.bankType;
   }

   public String getFeeType() {
      return this.feeType;
   }

   public String getTotalFee() {
      return this.totalFee;
   }

   public String getCouponFee() {
      return this.couponFee;
   }

   public String getRefundApplyDate() {
      return this.refundApplyDate;
   }

   public String getRefundSuccessDate() {
      return this.refundSuccessDate;
   }

   public String getRefundId() {
      return this.refundId;
   }

   public String getOutRefundNo() {
      return this.outRefundNo;
   }

   public String getRefundFee() {
      return this.refundFee;
   }

   public String getCouponRefundFee() {
      return this.couponRefundFee;
   }

   public String getRefundChannel() {
      return this.refundChannel;
   }

   public String getRefundStatus() {
      return this.refundStatus;
   }

   public String getBody() {
      return this.body;
   }

   public String getAttach() {
      return this.attach;
   }

   public String getPoundageFee() {
      return this.poundageFee;
   }

   public String getRate() {
      return this.rate;
   }

   public String getBankOrderNo() {
      return this.bankOrderNo;
   }

   public String getTradeInfo() {
      return this.tradeInfo;
   }

   public Date getTradeAt() {
      return this.tradeAt;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public void setTransactionId(String transactionId) {
      this.transactionId = transactionId;
   }

   public void setOutTradeNo(String outTradeNo) {
      this.outTradeNo = outTradeNo;
   }

   public void setTradeStatus(String tradeStatus) {
      this.tradeStatus = tradeStatus;
   }

   public void setTradeTime(String tradeTime) {
      this.tradeTime = tradeTime;
   }

   public void setAppid(String appid) {
      this.appid = appid;
   }

   public void setMchId(String mchId) {
      this.mchId = mchId;
   }

   public void setSubMchId(String subMchId) {
      this.subMchId = subMchId;
   }

   public void setDeviceInfo(String deviceInfo) {
      this.deviceInfo = deviceInfo;
   }

   public void setOpenId(String openId) {
      this.openId = openId;
   }

   public void setTradeType(String tradeType) {
      this.tradeType = tradeType;
   }

   public void setBankType(String bankType) {
      this.bankType = bankType;
   }

   public void setFeeType(String feeType) {
      this.feeType = feeType;
   }

   public void setTotalFee(String totalFee) {
      this.totalFee = totalFee;
   }

   public void setCouponFee(String couponFee) {
      this.couponFee = couponFee;
   }

   public void setRefundApplyDate(String refundApplyDate) {
      this.refundApplyDate = refundApplyDate;
   }

   public void setRefundSuccessDate(String refundSuccessDate) {
      this.refundSuccessDate = refundSuccessDate;
   }

   public void setRefundId(String refundId) {
      this.refundId = refundId;
   }

   public void setOutRefundNo(String outRefundNo) {
      this.outRefundNo = outRefundNo;
   }

   public void setRefundFee(String refundFee) {
      this.refundFee = refundFee;
   }

   public void setCouponRefundFee(String couponRefundFee) {
      this.couponRefundFee = couponRefundFee;
   }

   public void setRefundChannel(String refundChannel) {
      this.refundChannel = refundChannel;
   }

   public void setRefundStatus(String refundStatus) {
      this.refundStatus = refundStatus;
   }

   public void setBody(String body) {
      this.body = body;
   }

   public void setAttach(String attach) {
      this.attach = attach;
   }

   public void setPoundageFee(String poundageFee) {
      this.poundageFee = poundageFee;
   }

   public void setRate(String rate) {
      this.rate = rate;
   }

   public void setBankOrderNo(String bankOrderNo) {
      this.bankOrderNo = bankOrderNo;
   }

   public void setTradeInfo(String tradeInfo) {
      this.tradeInfo = tradeInfo;
   }

   public void setTradeAt(Date tradeAt) {
      this.tradeAt = tradeAt;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof MockWechatpayTrans)) {
         return false;
      } else {
         MockWechatpayTrans other = (MockWechatpayTrans)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$id = this.getId();
            Object other$id = other.getId();
            if(this$id == null) {
               if(other$id != null) {
                  return false;
               }
            } else if(!this$id.equals(other$id)) {
               return false;
            }

            Object this$transactionId = this.getTransactionId();
            Object other$transactionId = other.getTransactionId();
            if(this$transactionId == null) {
               if(other$transactionId != null) {
                  return false;
               }
            } else if(!this$transactionId.equals(other$transactionId)) {
               return false;
            }

            Object this$outTradeNo = this.getOutTradeNo();
            Object other$outTradeNo = other.getOutTradeNo();
            if(this$outTradeNo == null) {
               if(other$outTradeNo != null) {
                  return false;
               }
            } else if(!this$outTradeNo.equals(other$outTradeNo)) {
               return false;
            }

            Object this$tradeStatus = this.getTradeStatus();
            Object other$tradeStatus = other.getTradeStatus();
            if(this$tradeStatus == null) {
               if(other$tradeStatus != null) {
                  return false;
               }
            } else if(!this$tradeStatus.equals(other$tradeStatus)) {
               return false;
            }

            Object this$tradeTime = this.getTradeTime();
            Object other$tradeTime = other.getTradeTime();
            if(this$tradeTime == null) {
               if(other$tradeTime != null) {
                  return false;
               }
            } else if(!this$tradeTime.equals(other$tradeTime)) {
               return false;
            }

            Object this$appid = this.getAppid();
            Object other$appid = other.getAppid();
            if(this$appid == null) {
               if(other$appid != null) {
                  return false;
               }
            } else if(!this$appid.equals(other$appid)) {
               return false;
            }

            Object this$mchId = this.getMchId();
            Object other$mchId = other.getMchId();
            if(this$mchId == null) {
               if(other$mchId != null) {
                  return false;
               }
            } else if(!this$mchId.equals(other$mchId)) {
               return false;
            }

            Object this$subMchId = this.getSubMchId();
            Object other$subMchId = other.getSubMchId();
            if(this$subMchId == null) {
               if(other$subMchId != null) {
                  return false;
               }
            } else if(!this$subMchId.equals(other$subMchId)) {
               return false;
            }

            Object this$deviceInfo = this.getDeviceInfo();
            Object other$deviceInfo = other.getDeviceInfo();
            if(this$deviceInfo == null) {
               if(other$deviceInfo != null) {
                  return false;
               }
            } else if(!this$deviceInfo.equals(other$deviceInfo)) {
               return false;
            }

            Object this$openId = this.getOpenId();
            Object other$openId = other.getOpenId();
            if(this$openId == null) {
               if(other$openId != null) {
                  return false;
               }
            } else if(!this$openId.equals(other$openId)) {
               return false;
            }

            Object this$tradeType = this.getTradeType();
            Object other$tradeType = other.getTradeType();
            if(this$tradeType == null) {
               if(other$tradeType != null) {
                  return false;
               }
            } else if(!this$tradeType.equals(other$tradeType)) {
               return false;
            }

            Object this$bankType = this.getBankType();
            Object other$bankType = other.getBankType();
            if(this$bankType == null) {
               if(other$bankType != null) {
                  return false;
               }
            } else if(!this$bankType.equals(other$bankType)) {
               return false;
            }

            Object this$feeType = this.getFeeType();
            Object other$feeType = other.getFeeType();
            if(this$feeType == null) {
               if(other$feeType != null) {
                  return false;
               }
            } else if(!this$feeType.equals(other$feeType)) {
               return false;
            }

            Object this$totalFee = this.getTotalFee();
            Object other$totalFee = other.getTotalFee();
            if(this$totalFee == null) {
               if(other$totalFee != null) {
                  return false;
               }
            } else if(!this$totalFee.equals(other$totalFee)) {
               return false;
            }

            Object this$couponFee = this.getCouponFee();
            Object other$couponFee = other.getCouponFee();
            if(this$couponFee == null) {
               if(other$couponFee != null) {
                  return false;
               }
            } else if(!this$couponFee.equals(other$couponFee)) {
               return false;
            }

            Object this$refundApplyDate = this.getRefundApplyDate();
            Object other$refundApplyDate = other.getRefundApplyDate();
            if(this$refundApplyDate == null) {
               if(other$refundApplyDate != null) {
                  return false;
               }
            } else if(!this$refundApplyDate.equals(other$refundApplyDate)) {
               return false;
            }

            Object this$refundSuccessDate = this.getRefundSuccessDate();
            Object other$refundSuccessDate = other.getRefundSuccessDate();
            if(this$refundSuccessDate == null) {
               if(other$refundSuccessDate != null) {
                  return false;
               }
            } else if(!this$refundSuccessDate.equals(other$refundSuccessDate)) {
               return false;
            }

            Object this$refundId = this.getRefundId();
            Object other$refundId = other.getRefundId();
            if(this$refundId == null) {
               if(other$refundId != null) {
                  return false;
               }
            } else if(!this$refundId.equals(other$refundId)) {
               return false;
            }

            Object this$outRefundNo = this.getOutRefundNo();
            Object other$outRefundNo = other.getOutRefundNo();
            if(this$outRefundNo == null) {
               if(other$outRefundNo != null) {
                  return false;
               }
            } else if(!this$outRefundNo.equals(other$outRefundNo)) {
               return false;
            }

            Object this$refundFee = this.getRefundFee();
            Object other$refundFee = other.getRefundFee();
            if(this$refundFee == null) {
               if(other$refundFee != null) {
                  return false;
               }
            } else if(!this$refundFee.equals(other$refundFee)) {
               return false;
            }

            Object this$couponRefundFee = this.getCouponRefundFee();
            Object other$couponRefundFee = other.getCouponRefundFee();
            if(this$couponRefundFee == null) {
               if(other$couponRefundFee != null) {
                  return false;
               }
            } else if(!this$couponRefundFee.equals(other$couponRefundFee)) {
               return false;
            }

            Object this$refundChannel = this.getRefundChannel();
            Object other$refundChannel = other.getRefundChannel();
            if(this$refundChannel == null) {
               if(other$refundChannel != null) {
                  return false;
               }
            } else if(!this$refundChannel.equals(other$refundChannel)) {
               return false;
            }

            Object this$refundStatus = this.getRefundStatus();
            Object other$refundStatus = other.getRefundStatus();
            if(this$refundStatus == null) {
               if(other$refundStatus != null) {
                  return false;
               }
            } else if(!this$refundStatus.equals(other$refundStatus)) {
               return false;
            }

            Object this$body = this.getBody();
            Object other$body = other.getBody();
            if(this$body == null) {
               if(other$body != null) {
                  return false;
               }
            } else if(!this$body.equals(other$body)) {
               return false;
            }

            Object this$attach = this.getAttach();
            Object other$attach = other.getAttach();
            if(this$attach == null) {
               if(other$attach != null) {
                  return false;
               }
            } else if(!this$attach.equals(other$attach)) {
               return false;
            }

            Object this$poundageFee = this.getPoundageFee();
            Object other$poundageFee = other.getPoundageFee();
            if(this$poundageFee == null) {
               if(other$poundageFee != null) {
                  return false;
               }
            } else if(!this$poundageFee.equals(other$poundageFee)) {
               return false;
            }

            Object this$rate = this.getRate();
            Object other$rate = other.getRate();
            if(this$rate == null) {
               if(other$rate != null) {
                  return false;
               }
            } else if(!this$rate.equals(other$rate)) {
               return false;
            }

            Object this$bankOrderNo = this.getBankOrderNo();
            Object other$bankOrderNo = other.getBankOrderNo();
            if(this$bankOrderNo == null) {
               if(other$bankOrderNo != null) {
                  return false;
               }
            } else if(!this$bankOrderNo.equals(other$bankOrderNo)) {
               return false;
            }

            Object this$tradeInfo = this.getTradeInfo();
            Object other$tradeInfo = other.getTradeInfo();
            if(this$tradeInfo == null) {
               if(other$tradeInfo != null) {
                  return false;
               }
            } else if(!this$tradeInfo.equals(other$tradeInfo)) {
               return false;
            }

            Object this$tradeAt = this.getTradeAt();
            Object other$tradeAt = other.getTradeAt();
            if(this$tradeAt == null) {
               if(other$tradeAt != null) {
                  return false;
               }
            } else if(!this$tradeAt.equals(other$tradeAt)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof MockWechatpayTrans;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $transactionId = this.getTransactionId();
      result = result * 31 + ($transactionId == null?0:$transactionId.hashCode());
      Object $outTradeNo = this.getOutTradeNo();
      result = result * 31 + ($outTradeNo == null?0:$outTradeNo.hashCode());
      Object $tradeStatus = this.getTradeStatus();
      result = result * 31 + ($tradeStatus == null?0:$tradeStatus.hashCode());
      Object $tradeTime = this.getTradeTime();
      result = result * 31 + ($tradeTime == null?0:$tradeTime.hashCode());
      Object $appid = this.getAppid();
      result = result * 31 + ($appid == null?0:$appid.hashCode());
      Object $mchId = this.getMchId();
      result = result * 31 + ($mchId == null?0:$mchId.hashCode());
      Object $subMchId = this.getSubMchId();
      result = result * 31 + ($subMchId == null?0:$subMchId.hashCode());
      Object $deviceInfo = this.getDeviceInfo();
      result = result * 31 + ($deviceInfo == null?0:$deviceInfo.hashCode());
      Object $openId = this.getOpenId();
      result = result * 31 + ($openId == null?0:$openId.hashCode());
      Object $tradeType = this.getTradeType();
      result = result * 31 + ($tradeType == null?0:$tradeType.hashCode());
      Object $bankType = this.getBankType();
      result = result * 31 + ($bankType == null?0:$bankType.hashCode());
      Object $feeType = this.getFeeType();
      result = result * 31 + ($feeType == null?0:$feeType.hashCode());
      Object $totalFee = this.getTotalFee();
      result = result * 31 + ($totalFee == null?0:$totalFee.hashCode());
      Object $couponFee = this.getCouponFee();
      result = result * 31 + ($couponFee == null?0:$couponFee.hashCode());
      Object $refundApplyDate = this.getRefundApplyDate();
      result = result * 31 + ($refundApplyDate == null?0:$refundApplyDate.hashCode());
      Object $refundSuccessDate = this.getRefundSuccessDate();
      result = result * 31 + ($refundSuccessDate == null?0:$refundSuccessDate.hashCode());
      Object $refundId = this.getRefundId();
      result = result * 31 + ($refundId == null?0:$refundId.hashCode());
      Object $outRefundNo = this.getOutRefundNo();
      result = result * 31 + ($outRefundNo == null?0:$outRefundNo.hashCode());
      Object $refundFee = this.getRefundFee();
      result = result * 31 + ($refundFee == null?0:$refundFee.hashCode());
      Object $couponRefundFee = this.getCouponRefundFee();
      result = result * 31 + ($couponRefundFee == null?0:$couponRefundFee.hashCode());
      Object $refundChannel = this.getRefundChannel();
      result = result * 31 + ($refundChannel == null?0:$refundChannel.hashCode());
      Object $refundStatus = this.getRefundStatus();
      result = result * 31 + ($refundStatus == null?0:$refundStatus.hashCode());
      Object $body = this.getBody();
      result = result * 31 + ($body == null?0:$body.hashCode());
      Object $attach = this.getAttach();
      result = result * 31 + ($attach == null?0:$attach.hashCode());
      Object $poundageFee = this.getPoundageFee();
      result = result * 31 + ($poundageFee == null?0:$poundageFee.hashCode());
      Object $rate = this.getRate();
      result = result * 31 + ($rate == null?0:$rate.hashCode());
      Object $bankOrderNo = this.getBankOrderNo();
      result = result * 31 + ($bankOrderNo == null?0:$bankOrderNo.hashCode());
      Object $tradeInfo = this.getTradeInfo();
      result = result * 31 + ($tradeInfo == null?0:$tradeInfo.hashCode());
      Object $tradeAt = this.getTradeAt();
      result = result * 31 + ($tradeAt == null?0:$tradeAt.hashCode());
      return result;
   }

   public String toString() {
      return "MockWechatpayTrans(id=" + this.getId() + ", transactionId=" + this.getTransactionId() + ", outTradeNo=" + this.getOutTradeNo() + ", tradeStatus=" + this.getTradeStatus() + ", tradeTime=" + this.getTradeTime() + ", appid=" + this.getAppid() + ", mchId=" + this.getMchId() + ", subMchId=" + this.getSubMchId() + ", deviceInfo=" + this.getDeviceInfo() + ", openId=" + this.getOpenId() + ", tradeType=" + this.getTradeType() + ", bankType=" + this.getBankType() + ", feeType=" + this.getFeeType() + ", totalFee=" + this.getTotalFee() + ", couponFee=" + this.getCouponFee() + ", refundApplyDate=" + this.getRefundApplyDate() + ", refundSuccessDate=" + this.getRefundSuccessDate() + ", refundId=" + this.getRefundId() + ", outRefundNo=" + this.getOutRefundNo() + ", refundFee=" + this.getRefundFee() + ", couponRefundFee=" + this.getCouponRefundFee() + ", refundChannel=" + this.getRefundChannel() + ", refundStatus=" + this.getRefundStatus() + ", body=" + this.getBody() + ", attach=" + this.getAttach() + ", poundageFee=" + this.getPoundageFee() + ", rate=" + this.getRate() + ", bankOrderNo=" + this.getBankOrderNo() + ", tradeInfo=" + this.getTradeInfo() + ", tradeAt=" + this.getTradeAt() + ")";
   }
}
