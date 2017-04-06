package cn.blmdz.wolf.pay.model;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

public class UnionPayTrans implements Serializable {
   private static final long serialVersionUID = -2660985544430867518L;
   private Long id;
   private String transactionCode;
   private String acqInsCode;
   private String sendCode;
   private String traceNo;
   private Date txnTime;
   private String payCardNo;
   private String txnAmt;
   private String merCatCode;
   private String termType;
   private String queryId;
   private String type;
   private String orderId;
   private String payCardType;
   private String originalTraceNo;
   private String originalTime;
   private String thirdPartyFee;
   private String settleAmount;
   private String payType;
   private String companyCode;
   private String txnType;
   private String txnSubType;
   private String bizType;
   private String accType;
   private String billType;
   private String billNo;
   private String interactMode;
   private String origQryId;
   private String merId;
   private String divideType;
   private String subMerId;
   private String subMerAbbr;
   private String divideAmount;
   private String clearing;
   private String termId;
   private String merReserved;
   private String discount;
   private String invoice;
   private String additionThirdPartyFee;
   private String stage;
   private String transactionMedia;
   private String originalOrderId;
   private Date createdAt;
   private Date updatedAt;

   public Long getTxnAmt() {
      if(this.txnAmt == null) {
         return Long.valueOf(0L);
      } else {
         BigDecimal money = new BigDecimal(this.txnAmt);
         return Long.valueOf(money.longValue());
      }
   }

   public Long getThirdPartyFee() {
      if(this.thirdPartyFee != null && !this.thirdPartyFee.equals("")) {
         BigDecimal money = new BigDecimal(this.thirdPartyFee);
         return Long.valueOf(money.longValue());
      } else {
         return Long.valueOf(0L);
      }
   }

   public Long getId() {
      return this.id;
   }

   public String getTransactionCode() {
      return this.transactionCode;
   }

   public String getAcqInsCode() {
      return this.acqInsCode;
   }

   public String getSendCode() {
      return this.sendCode;
   }

   public String getTraceNo() {
      return this.traceNo;
   }

   public Date getTxnTime() {
      return this.txnTime;
   }

   public String getPayCardNo() {
      return this.payCardNo;
   }

   public String getMerCatCode() {
      return this.merCatCode;
   }

   public String getTermType() {
      return this.termType;
   }

   public String getQueryId() {
      return this.queryId;
   }

   public String getType() {
      return this.type;
   }

   public String getOrderId() {
      return this.orderId;
   }

   public String getPayCardType() {
      return this.payCardType;
   }

   public String getOriginalTraceNo() {
      return this.originalTraceNo;
   }

   public String getOriginalTime() {
      return this.originalTime;
   }

   public String getSettleAmount() {
      return this.settleAmount;
   }

   public String getPayType() {
      return this.payType;
   }

   public String getCompanyCode() {
      return this.companyCode;
   }

   public String getTxnType() {
      return this.txnType;
   }

   public String getTxnSubType() {
      return this.txnSubType;
   }

   public String getBizType() {
      return this.bizType;
   }

   public String getAccType() {
      return this.accType;
   }

   public String getBillType() {
      return this.billType;
   }

   public String getBillNo() {
      return this.billNo;
   }

   public String getInteractMode() {
      return this.interactMode;
   }

   public String getOrigQryId() {
      return this.origQryId;
   }

   public String getMerId() {
      return this.merId;
   }

   public String getDivideType() {
      return this.divideType;
   }

   public String getSubMerId() {
      return this.subMerId;
   }

   public String getSubMerAbbr() {
      return this.subMerAbbr;
   }

   public String getDivideAmount() {
      return this.divideAmount;
   }

   public String getClearing() {
      return this.clearing;
   }

   public String getTermId() {
      return this.termId;
   }

   public String getMerReserved() {
      return this.merReserved;
   }

   public String getDiscount() {
      return this.discount;
   }

   public String getInvoice() {
      return this.invoice;
   }

   public String getAdditionThirdPartyFee() {
      return this.additionThirdPartyFee;
   }

   public String getStage() {
      return this.stage;
   }

   public String getTransactionMedia() {
      return this.transactionMedia;
   }

   public String getOriginalOrderId() {
      return this.originalOrderId;
   }

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public void setTransactionCode(String transactionCode) {
      this.transactionCode = transactionCode;
   }

   public void setAcqInsCode(String acqInsCode) {
      this.acqInsCode = acqInsCode;
   }

   public void setSendCode(String sendCode) {
      this.sendCode = sendCode;
   }

   public void setTraceNo(String traceNo) {
      this.traceNo = traceNo;
   }

   public void setTxnTime(Date txnTime) {
      this.txnTime = txnTime;
   }

   public void setPayCardNo(String payCardNo) {
      this.payCardNo = payCardNo;
   }

   public void setTxnAmt(String txnAmt) {
      this.txnAmt = txnAmt;
   }

   public void setMerCatCode(String merCatCode) {
      this.merCatCode = merCatCode;
   }

   public void setTermType(String termType) {
      this.termType = termType;
   }

   public void setQueryId(String queryId) {
      this.queryId = queryId;
   }

   public void setType(String type) {
      this.type = type;
   }

   public void setOrderId(String orderId) {
      this.orderId = orderId;
   }

   public void setPayCardType(String payCardType) {
      this.payCardType = payCardType;
   }

   public void setOriginalTraceNo(String originalTraceNo) {
      this.originalTraceNo = originalTraceNo;
   }

   public void setOriginalTime(String originalTime) {
      this.originalTime = originalTime;
   }

   public void setThirdPartyFee(String thirdPartyFee) {
      this.thirdPartyFee = thirdPartyFee;
   }

   public void setSettleAmount(String settleAmount) {
      this.settleAmount = settleAmount;
   }

   public void setPayType(String payType) {
      this.payType = payType;
   }

   public void setCompanyCode(String companyCode) {
      this.companyCode = companyCode;
   }

   public void setTxnType(String txnType) {
      this.txnType = txnType;
   }

   public void setTxnSubType(String txnSubType) {
      this.txnSubType = txnSubType;
   }

   public void setBizType(String bizType) {
      this.bizType = bizType;
   }

   public void setAccType(String accType) {
      this.accType = accType;
   }

   public void setBillType(String billType) {
      this.billType = billType;
   }

   public void setBillNo(String billNo) {
      this.billNo = billNo;
   }

   public void setInteractMode(String interactMode) {
      this.interactMode = interactMode;
   }

   public void setOrigQryId(String origQryId) {
      this.origQryId = origQryId;
   }

   public void setMerId(String merId) {
      this.merId = merId;
   }

   public void setDivideType(String divideType) {
      this.divideType = divideType;
   }

   public void setSubMerId(String subMerId) {
      this.subMerId = subMerId;
   }

   public void setSubMerAbbr(String subMerAbbr) {
      this.subMerAbbr = subMerAbbr;
   }

   public void setDivideAmount(String divideAmount) {
      this.divideAmount = divideAmount;
   }

   public void setClearing(String clearing) {
      this.clearing = clearing;
   }

   public void setTermId(String termId) {
      this.termId = termId;
   }

   public void setMerReserved(String merReserved) {
      this.merReserved = merReserved;
   }

   public void setDiscount(String discount) {
      this.discount = discount;
   }

   public void setInvoice(String invoice) {
      this.invoice = invoice;
   }

   public void setAdditionThirdPartyFee(String additionThirdPartyFee) {
      this.additionThirdPartyFee = additionThirdPartyFee;
   }

   public void setStage(String stage) {
      this.stage = stage;
   }

   public void setTransactionMedia(String transactionMedia) {
      this.transactionMedia = transactionMedia;
   }

   public void setOriginalOrderId(String originalOrderId) {
      this.originalOrderId = originalOrderId;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof UnionPayTrans)) {
         return false;
      } else {
         UnionPayTrans other = (UnionPayTrans)o;
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

            Object this$transactionCode = this.getTransactionCode();
            Object other$transactionCode = other.getTransactionCode();
            if(this$transactionCode == null) {
               if(other$transactionCode != null) {
                  return false;
               }
            } else if(!this$transactionCode.equals(other$transactionCode)) {
               return false;
            }

            Object this$acqInsCode = this.getAcqInsCode();
            Object other$acqInsCode = other.getAcqInsCode();
            if(this$acqInsCode == null) {
               if(other$acqInsCode != null) {
                  return false;
               }
            } else if(!this$acqInsCode.equals(other$acqInsCode)) {
               return false;
            }

            Object this$sendCode = this.getSendCode();
            Object other$sendCode = other.getSendCode();
            if(this$sendCode == null) {
               if(other$sendCode != null) {
                  return false;
               }
            } else if(!this$sendCode.equals(other$sendCode)) {
               return false;
            }

            Object this$traceNo = this.getTraceNo();
            Object other$traceNo = other.getTraceNo();
            if(this$traceNo == null) {
               if(other$traceNo != null) {
                  return false;
               }
            } else if(!this$traceNo.equals(other$traceNo)) {
               return false;
            }

            Object this$txnTime = this.getTxnTime();
            Object other$txnTime = other.getTxnTime();
            if(this$txnTime == null) {
               if(other$txnTime != null) {
                  return false;
               }
            } else if(!this$txnTime.equals(other$txnTime)) {
               return false;
            }

            Object this$payCardNo = this.getPayCardNo();
            Object other$payCardNo = other.getPayCardNo();
            if(this$payCardNo == null) {
               if(other$payCardNo != null) {
                  return false;
               }
            } else if(!this$payCardNo.equals(other$payCardNo)) {
               return false;
            }

            Object this$txnAmt = this.getTxnAmt();
            Object other$txnAmt = other.getTxnAmt();
            if(this$txnAmt == null) {
               if(other$txnAmt != null) {
                  return false;
               }
            } else if(!this$txnAmt.equals(other$txnAmt)) {
               return false;
            }

            Object this$merCatCode = this.getMerCatCode();
            Object other$merCatCode = other.getMerCatCode();
            if(this$merCatCode == null) {
               if(other$merCatCode != null) {
                  return false;
               }
            } else if(!this$merCatCode.equals(other$merCatCode)) {
               return false;
            }

            Object this$termType = this.getTermType();
            Object other$termType = other.getTermType();
            if(this$termType == null) {
               if(other$termType != null) {
                  return false;
               }
            } else if(!this$termType.equals(other$termType)) {
               return false;
            }

            Object this$queryId = this.getQueryId();
            Object other$queryId = other.getQueryId();
            if(this$queryId == null) {
               if(other$queryId != null) {
                  return false;
               }
            } else if(!this$queryId.equals(other$queryId)) {
               return false;
            }

            Object this$type = this.getType();
            Object other$type = other.getType();
            if(this$type == null) {
               if(other$type != null) {
                  return false;
               }
            } else if(!this$type.equals(other$type)) {
               return false;
            }

            Object this$orderId = this.getOrderId();
            Object other$orderId = other.getOrderId();
            if(this$orderId == null) {
               if(other$orderId != null) {
                  return false;
               }
            } else if(!this$orderId.equals(other$orderId)) {
               return false;
            }

            Object this$payCardType = this.getPayCardType();
            Object other$payCardType = other.getPayCardType();
            if(this$payCardType == null) {
               if(other$payCardType != null) {
                  return false;
               }
            } else if(!this$payCardType.equals(other$payCardType)) {
               return false;
            }

            Object this$originalTraceNo = this.getOriginalTraceNo();
            Object other$originalTraceNo = other.getOriginalTraceNo();
            if(this$originalTraceNo == null) {
               if(other$originalTraceNo != null) {
                  return false;
               }
            } else if(!this$originalTraceNo.equals(other$originalTraceNo)) {
               return false;
            }

            Object this$originalTime = this.getOriginalTime();
            Object other$originalTime = other.getOriginalTime();
            if(this$originalTime == null) {
               if(other$originalTime != null) {
                  return false;
               }
            } else if(!this$originalTime.equals(other$originalTime)) {
               return false;
            }

            Object this$thirdPartyFee = this.getThirdPartyFee();
            Object other$thirdPartyFee = other.getThirdPartyFee();
            if(this$thirdPartyFee == null) {
               if(other$thirdPartyFee != null) {
                  return false;
               }
            } else if(!this$thirdPartyFee.equals(other$thirdPartyFee)) {
               return false;
            }

            Object this$settleAmount = this.getSettleAmount();
            Object other$settleAmount = other.getSettleAmount();
            if(this$settleAmount == null) {
               if(other$settleAmount != null) {
                  return false;
               }
            } else if(!this$settleAmount.equals(other$settleAmount)) {
               return false;
            }

            Object this$payType = this.getPayType();
            Object other$payType = other.getPayType();
            if(this$payType == null) {
               if(other$payType != null) {
                  return false;
               }
            } else if(!this$payType.equals(other$payType)) {
               return false;
            }

            Object this$companyCode = this.getCompanyCode();
            Object other$companyCode = other.getCompanyCode();
            if(this$companyCode == null) {
               if(other$companyCode != null) {
                  return false;
               }
            } else if(!this$companyCode.equals(other$companyCode)) {
               return false;
            }

            Object this$txnType = this.getTxnType();
            Object other$txnType = other.getTxnType();
            if(this$txnType == null) {
               if(other$txnType != null) {
                  return false;
               }
            } else if(!this$txnType.equals(other$txnType)) {
               return false;
            }

            Object this$txnSubType = this.getTxnSubType();
            Object other$txnSubType = other.getTxnSubType();
            if(this$txnSubType == null) {
               if(other$txnSubType != null) {
                  return false;
               }
            } else if(!this$txnSubType.equals(other$txnSubType)) {
               return false;
            }

            Object this$bizType = this.getBizType();
            Object other$bizType = other.getBizType();
            if(this$bizType == null) {
               if(other$bizType != null) {
                  return false;
               }
            } else if(!this$bizType.equals(other$bizType)) {
               return false;
            }

            Object this$accType = this.getAccType();
            Object other$accType = other.getAccType();
            if(this$accType == null) {
               if(other$accType != null) {
                  return false;
               }
            } else if(!this$accType.equals(other$accType)) {
               return false;
            }

            Object this$billType = this.getBillType();
            Object other$billType = other.getBillType();
            if(this$billType == null) {
               if(other$billType != null) {
                  return false;
               }
            } else if(!this$billType.equals(other$billType)) {
               return false;
            }

            Object this$billNo = this.getBillNo();
            Object other$billNo = other.getBillNo();
            if(this$billNo == null) {
               if(other$billNo != null) {
                  return false;
               }
            } else if(!this$billNo.equals(other$billNo)) {
               return false;
            }

            Object this$interactMode = this.getInteractMode();
            Object other$interactMode = other.getInteractMode();
            if(this$interactMode == null) {
               if(other$interactMode != null) {
                  return false;
               }
            } else if(!this$interactMode.equals(other$interactMode)) {
               return false;
            }

            Object this$origQryId = this.getOrigQryId();
            Object other$origQryId = other.getOrigQryId();
            if(this$origQryId == null) {
               if(other$origQryId != null) {
                  return false;
               }
            } else if(!this$origQryId.equals(other$origQryId)) {
               return false;
            }

            Object this$merId = this.getMerId();
            Object other$merId = other.getMerId();
            if(this$merId == null) {
               if(other$merId != null) {
                  return false;
               }
            } else if(!this$merId.equals(other$merId)) {
               return false;
            }

            Object this$divideType = this.getDivideType();
            Object other$divideType = other.getDivideType();
            if(this$divideType == null) {
               if(other$divideType != null) {
                  return false;
               }
            } else if(!this$divideType.equals(other$divideType)) {
               return false;
            }

            Object this$subMerId = this.getSubMerId();
            Object other$subMerId = other.getSubMerId();
            if(this$subMerId == null) {
               if(other$subMerId != null) {
                  return false;
               }
            } else if(!this$subMerId.equals(other$subMerId)) {
               return false;
            }

            Object this$subMerAbbr = this.getSubMerAbbr();
            Object other$subMerAbbr = other.getSubMerAbbr();
            if(this$subMerAbbr == null) {
               if(other$subMerAbbr != null) {
                  return false;
               }
            } else if(!this$subMerAbbr.equals(other$subMerAbbr)) {
               return false;
            }

            Object this$divideAmount = this.getDivideAmount();
            Object other$divideAmount = other.getDivideAmount();
            if(this$divideAmount == null) {
               if(other$divideAmount != null) {
                  return false;
               }
            } else if(!this$divideAmount.equals(other$divideAmount)) {
               return false;
            }

            Object this$clearing = this.getClearing();
            Object other$clearing = other.getClearing();
            if(this$clearing == null) {
               if(other$clearing != null) {
                  return false;
               }
            } else if(!this$clearing.equals(other$clearing)) {
               return false;
            }

            Object this$termId = this.getTermId();
            Object other$termId = other.getTermId();
            if(this$termId == null) {
               if(other$termId != null) {
                  return false;
               }
            } else if(!this$termId.equals(other$termId)) {
               return false;
            }

            Object this$merReserved = this.getMerReserved();
            Object other$merReserved = other.getMerReserved();
            if(this$merReserved == null) {
               if(other$merReserved != null) {
                  return false;
               }
            } else if(!this$merReserved.equals(other$merReserved)) {
               return false;
            }

            Object this$discount = this.getDiscount();
            Object other$discount = other.getDiscount();
            if(this$discount == null) {
               if(other$discount != null) {
                  return false;
               }
            } else if(!this$discount.equals(other$discount)) {
               return false;
            }

            Object this$invoice = this.getInvoice();
            Object other$invoice = other.getInvoice();
            if(this$invoice == null) {
               if(other$invoice != null) {
                  return false;
               }
            } else if(!this$invoice.equals(other$invoice)) {
               return false;
            }

            Object this$additionThirdPartyFee = this.getAdditionThirdPartyFee();
            Object other$additionThirdPartyFee = other.getAdditionThirdPartyFee();
            if(this$additionThirdPartyFee == null) {
               if(other$additionThirdPartyFee != null) {
                  return false;
               }
            } else if(!this$additionThirdPartyFee.equals(other$additionThirdPartyFee)) {
               return false;
            }

            Object this$stage = this.getStage();
            Object other$stage = other.getStage();
            if(this$stage == null) {
               if(other$stage != null) {
                  return false;
               }
            } else if(!this$stage.equals(other$stage)) {
               return false;
            }

            Object this$transactionMedia = this.getTransactionMedia();
            Object other$transactionMedia = other.getTransactionMedia();
            if(this$transactionMedia == null) {
               if(other$transactionMedia != null) {
                  return false;
               }
            } else if(!this$transactionMedia.equals(other$transactionMedia)) {
               return false;
            }

            Object this$originalOrderId = this.getOriginalOrderId();
            Object other$originalOrderId = other.getOriginalOrderId();
            if(this$originalOrderId == null) {
               if(other$originalOrderId != null) {
                  return false;
               }
            } else if(!this$originalOrderId.equals(other$originalOrderId)) {
               return false;
            }

            Object this$createdAt = this.getCreatedAt();
            Object other$createdAt = other.getCreatedAt();
            if(this$createdAt == null) {
               if(other$createdAt != null) {
                  return false;
               }
            } else if(!this$createdAt.equals(other$createdAt)) {
               return false;
            }

            Object this$updatedAt = this.getUpdatedAt();
            Object other$updatedAt = other.getUpdatedAt();
            if(this$updatedAt == null) {
               if(other$updatedAt != null) {
                  return false;
               }
            } else if(!this$updatedAt.equals(other$updatedAt)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof UnionPayTrans;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $transactionCode = this.getTransactionCode();
      result = result * 31 + ($transactionCode == null?0:$transactionCode.hashCode());
      Object $acqInsCode = this.getAcqInsCode();
      result = result * 31 + ($acqInsCode == null?0:$acqInsCode.hashCode());
      Object $sendCode = this.getSendCode();
      result = result * 31 + ($sendCode == null?0:$sendCode.hashCode());
      Object $traceNo = this.getTraceNo();
      result = result * 31 + ($traceNo == null?0:$traceNo.hashCode());
      Object $txnTime = this.getTxnTime();
      result = result * 31 + ($txnTime == null?0:$txnTime.hashCode());
      Object $payCardNo = this.getPayCardNo();
      result = result * 31 + ($payCardNo == null?0:$payCardNo.hashCode());
      Object $txnAmt = this.getTxnAmt();
      result = result * 31 + ($txnAmt == null?0:$txnAmt.hashCode());
      Object $merCatCode = this.getMerCatCode();
      result = result * 31 + ($merCatCode == null?0:$merCatCode.hashCode());
      Object $termType = this.getTermType();
      result = result * 31 + ($termType == null?0:$termType.hashCode());
      Object $queryId = this.getQueryId();
      result = result * 31 + ($queryId == null?0:$queryId.hashCode());
      Object $type = this.getType();
      result = result * 31 + ($type == null?0:$type.hashCode());
      Object $orderId = this.getOrderId();
      result = result * 31 + ($orderId == null?0:$orderId.hashCode());
      Object $payCardType = this.getPayCardType();
      result = result * 31 + ($payCardType == null?0:$payCardType.hashCode());
      Object $originalTraceNo = this.getOriginalTraceNo();
      result = result * 31 + ($originalTraceNo == null?0:$originalTraceNo.hashCode());
      Object $originalTime = this.getOriginalTime();
      result = result * 31 + ($originalTime == null?0:$originalTime.hashCode());
      Object $thirdPartyFee = this.getThirdPartyFee();
      result = result * 31 + ($thirdPartyFee == null?0:$thirdPartyFee.hashCode());
      Object $settleAmount = this.getSettleAmount();
      result = result * 31 + ($settleAmount == null?0:$settleAmount.hashCode());
      Object $payType = this.getPayType();
      result = result * 31 + ($payType == null?0:$payType.hashCode());
      Object $companyCode = this.getCompanyCode();
      result = result * 31 + ($companyCode == null?0:$companyCode.hashCode());
      Object $txnType = this.getTxnType();
      result = result * 31 + ($txnType == null?0:$txnType.hashCode());
      Object $txnSubType = this.getTxnSubType();
      result = result * 31 + ($txnSubType == null?0:$txnSubType.hashCode());
      Object $bizType = this.getBizType();
      result = result * 31 + ($bizType == null?0:$bizType.hashCode());
      Object $accType = this.getAccType();
      result = result * 31 + ($accType == null?0:$accType.hashCode());
      Object $billType = this.getBillType();
      result = result * 31 + ($billType == null?0:$billType.hashCode());
      Object $billNo = this.getBillNo();
      result = result * 31 + ($billNo == null?0:$billNo.hashCode());
      Object $interactMode = this.getInteractMode();
      result = result * 31 + ($interactMode == null?0:$interactMode.hashCode());
      Object $origQryId = this.getOrigQryId();
      result = result * 31 + ($origQryId == null?0:$origQryId.hashCode());
      Object $merId = this.getMerId();
      result = result * 31 + ($merId == null?0:$merId.hashCode());
      Object $divideType = this.getDivideType();
      result = result * 31 + ($divideType == null?0:$divideType.hashCode());
      Object $subMerId = this.getSubMerId();
      result = result * 31 + ($subMerId == null?0:$subMerId.hashCode());
      Object $subMerAbbr = this.getSubMerAbbr();
      result = result * 31 + ($subMerAbbr == null?0:$subMerAbbr.hashCode());
      Object $divideAmount = this.getDivideAmount();
      result = result * 31 + ($divideAmount == null?0:$divideAmount.hashCode());
      Object $clearing = this.getClearing();
      result = result * 31 + ($clearing == null?0:$clearing.hashCode());
      Object $termId = this.getTermId();
      result = result * 31 + ($termId == null?0:$termId.hashCode());
      Object $merReserved = this.getMerReserved();
      result = result * 31 + ($merReserved == null?0:$merReserved.hashCode());
      Object $discount = this.getDiscount();
      result = result * 31 + ($discount == null?0:$discount.hashCode());
      Object $invoice = this.getInvoice();
      result = result * 31 + ($invoice == null?0:$invoice.hashCode());
      Object $additionThirdPartyFee = this.getAdditionThirdPartyFee();
      result = result * 31 + ($additionThirdPartyFee == null?0:$additionThirdPartyFee.hashCode());
      Object $stage = this.getStage();
      result = result * 31 + ($stage == null?0:$stage.hashCode());
      Object $transactionMedia = this.getTransactionMedia();
      result = result * 31 + ($transactionMedia == null?0:$transactionMedia.hashCode());
      Object $originalOrderId = this.getOriginalOrderId();
      result = result * 31 + ($originalOrderId == null?0:$originalOrderId.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "UnionPayTrans(id=" + this.getId() + ", transactionCode=" + this.getTransactionCode() + ", acqInsCode=" + this.getAcqInsCode() + ", sendCode=" + this.getSendCode() + ", traceNo=" + this.getTraceNo() + ", txnTime=" + this.getTxnTime() + ", payCardNo=" + this.getPayCardNo() + ", txnAmt=" + this.getTxnAmt() + ", merCatCode=" + this.getMerCatCode() + ", termType=" + this.getTermType() + ", queryId=" + this.getQueryId() + ", type=" + this.getType() + ", orderId=" + this.getOrderId() + ", payCardType=" + this.getPayCardType() + ", originalTraceNo=" + this.getOriginalTraceNo() + ", originalTime=" + this.getOriginalTime() + ", thirdPartyFee=" + this.getThirdPartyFee() + ", settleAmount=" + this.getSettleAmount() + ", payType=" + this.getPayType() + ", companyCode=" + this.getCompanyCode() + ", txnType=" + this.getTxnType() + ", txnSubType=" + this.getTxnSubType() + ", bizType=" + this.getBizType() + ", accType=" + this.getAccType() + ", billType=" + this.getBillType() + ", billNo=" + this.getBillNo() + ", interactMode=" + this.getInteractMode() + ", origQryId=" + this.getOrigQryId() + ", merId=" + this.getMerId() + ", divideType=" + this.getDivideType() + ", subMerId=" + this.getSubMerId() + ", subMerAbbr=" + this.getSubMerAbbr() + ", divideAmount=" + this.getDivideAmount() + ", clearing=" + this.getClearing() + ", termId=" + this.getTermId() + ", merReserved=" + this.getMerReserved() + ", discount=" + this.getDiscount() + ", invoice=" + this.getInvoice() + ", additionThirdPartyFee=" + this.getAdditionThirdPartyFee() + ", stage=" + this.getStage() + ", transactionMedia=" + this.getTransactionMedia() + ", originalOrderId=" + this.getOriginalOrderId() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
