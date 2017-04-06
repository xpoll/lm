package cn.blmdz.wolf.pay.model;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

public class AlipayTrans implements Serializable {
   private static final long serialVersionUID = 8700143474959118663L;
   private static final BigDecimal ratio = new BigDecimal("100");
   private Long id;
   protected String balance;
   protected String bankAccountName;
   protected String bankAccountNo;
   protected String bankName;
   protected String buyerName;
   protected String buyerAccount;
   protected String currency;
   protected String depositBankNo;
   protected String goodsTitle;
   protected String income;
   protected String iwAccountLogId;
   protected String memo;
   protected String merchantOutOrderNo;
   protected String otherAccountEmail;
   protected String otherAccountFullname;
   protected String otherUserId;
   protected String outcome;
   protected String partnerId;
   protected String sellerAccount;
   protected String sellerFullname;
   protected String serviceFee;
   protected String serviceFeeRatio;
   protected String totalFee;
   protected String tradeNo;
   protected String tradeRefundAmount;
   protected String transAccount;
   protected String transCodeMsg;
   protected String transDate;
   protected String transOutOrderNo;
   protected String subTransCodeMsg;
   protected String signProductName;
   protected String rate;
   private Date tradeAt;
   private Date createdAt;
   private Date updatedAt;

   public Long outcomeToFen() {
      if(this.outcome == null) {
         return Long.valueOf(0L);
      } else {
         BigDecimal money = new BigDecimal(this.outcome);
         return Long.valueOf(money.multiply(ratio).longValue());
      }
   }

   public Long incomeToFen() {
      if(this.income == null) {
         return Long.valueOf(0L);
      } else {
         BigDecimal money = new BigDecimal(this.income);
         return Long.valueOf(money.multiply(ratio).longValue());
      }
   }

   public Long totalFeeToFen() {
      if(this.totalFee == null) {
         return Long.valueOf(0L);
      } else {
         BigDecimal money = new BigDecimal(this.totalFee);
         return Long.valueOf(money.multiply(ratio).longValue());
      }
   }

   public Long getId() {
      return this.id;
   }

   public String getBalance() {
      return this.balance;
   }

   public String getBankAccountName() {
      return this.bankAccountName;
   }

   public String getBankAccountNo() {
      return this.bankAccountNo;
   }

   public String getBankName() {
      return this.bankName;
   }

   public String getBuyerName() {
      return this.buyerName;
   }

   public String getBuyerAccount() {
      return this.buyerAccount;
   }

   public String getCurrency() {
      return this.currency;
   }

   public String getDepositBankNo() {
      return this.depositBankNo;
   }

   public String getGoodsTitle() {
      return this.goodsTitle;
   }

   public String getIncome() {
      return this.income;
   }

   public String getIwAccountLogId() {
      return this.iwAccountLogId;
   }

   public String getMemo() {
      return this.memo;
   }

   public String getMerchantOutOrderNo() {
      return this.merchantOutOrderNo;
   }

   public String getOtherAccountEmail() {
      return this.otherAccountEmail;
   }

   public String getOtherAccountFullname() {
      return this.otherAccountFullname;
   }

   public String getOtherUserId() {
      return this.otherUserId;
   }

   public String getOutcome() {
      return this.outcome;
   }

   public String getPartnerId() {
      return this.partnerId;
   }

   public String getSellerAccount() {
      return this.sellerAccount;
   }

   public String getSellerFullname() {
      return this.sellerFullname;
   }

   public String getServiceFee() {
      return this.serviceFee;
   }

   public String getServiceFeeRatio() {
      return this.serviceFeeRatio;
   }

   public String getTotalFee() {
      return this.totalFee;
   }

   public String getTradeNo() {
      return this.tradeNo;
   }

   public String getTradeRefundAmount() {
      return this.tradeRefundAmount;
   }

   public String getTransAccount() {
      return this.transAccount;
   }

   public String getTransCodeMsg() {
      return this.transCodeMsg;
   }

   public String getTransDate() {
      return this.transDate;
   }

   public String getTransOutOrderNo() {
      return this.transOutOrderNo;
   }

   public String getSubTransCodeMsg() {
      return this.subTransCodeMsg;
   }

   public String getSignProductName() {
      return this.signProductName;
   }

   public String getRate() {
      return this.rate;
   }

   public Date getTradeAt() {
      return this.tradeAt;
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

   public void setBalance(String balance) {
      this.balance = balance;
   }

   public void setBankAccountName(String bankAccountName) {
      this.bankAccountName = bankAccountName;
   }

   public void setBankAccountNo(String bankAccountNo) {
      this.bankAccountNo = bankAccountNo;
   }

   public void setBankName(String bankName) {
      this.bankName = bankName;
   }

   public void setBuyerName(String buyerName) {
      this.buyerName = buyerName;
   }

   public void setBuyerAccount(String buyerAccount) {
      this.buyerAccount = buyerAccount;
   }

   public void setCurrency(String currency) {
      this.currency = currency;
   }

   public void setDepositBankNo(String depositBankNo) {
      this.depositBankNo = depositBankNo;
   }

   public void setGoodsTitle(String goodsTitle) {
      this.goodsTitle = goodsTitle;
   }

   public void setIncome(String income) {
      this.income = income;
   }

   public void setIwAccountLogId(String iwAccountLogId) {
      this.iwAccountLogId = iwAccountLogId;
   }

   public void setMemo(String memo) {
      this.memo = memo;
   }

   public void setMerchantOutOrderNo(String merchantOutOrderNo) {
      this.merchantOutOrderNo = merchantOutOrderNo;
   }

   public void setOtherAccountEmail(String otherAccountEmail) {
      this.otherAccountEmail = otherAccountEmail;
   }

   public void setOtherAccountFullname(String otherAccountFullname) {
      this.otherAccountFullname = otherAccountFullname;
   }

   public void setOtherUserId(String otherUserId) {
      this.otherUserId = otherUserId;
   }

   public void setOutcome(String outcome) {
      this.outcome = outcome;
   }

   public void setPartnerId(String partnerId) {
      this.partnerId = partnerId;
   }

   public void setSellerAccount(String sellerAccount) {
      this.sellerAccount = sellerAccount;
   }

   public void setSellerFullname(String sellerFullname) {
      this.sellerFullname = sellerFullname;
   }

   public void setServiceFee(String serviceFee) {
      this.serviceFee = serviceFee;
   }

   public void setServiceFeeRatio(String serviceFeeRatio) {
      this.serviceFeeRatio = serviceFeeRatio;
   }

   public void setTotalFee(String totalFee) {
      this.totalFee = totalFee;
   }

   public void setTradeNo(String tradeNo) {
      this.tradeNo = tradeNo;
   }

   public void setTradeRefundAmount(String tradeRefundAmount) {
      this.tradeRefundAmount = tradeRefundAmount;
   }

   public void setTransAccount(String transAccount) {
      this.transAccount = transAccount;
   }

   public void setTransCodeMsg(String transCodeMsg) {
      this.transCodeMsg = transCodeMsg;
   }

   public void setTransDate(String transDate) {
      this.transDate = transDate;
   }

   public void setTransOutOrderNo(String transOutOrderNo) {
      this.transOutOrderNo = transOutOrderNo;
   }

   public void setSubTransCodeMsg(String subTransCodeMsg) {
      this.subTransCodeMsg = subTransCodeMsg;
   }

   public void setSignProductName(String signProductName) {
      this.signProductName = signProductName;
   }

   public void setRate(String rate) {
      this.rate = rate;
   }

   public void setTradeAt(Date tradeAt) {
      this.tradeAt = tradeAt;
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
      } else if(!(o instanceof AlipayTrans)) {
         return false;
      } else {
         AlipayTrans other = (AlipayTrans)o;
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

            Object this$balance = this.getBalance();
            Object other$balance = other.getBalance();
            if(this$balance == null) {
               if(other$balance != null) {
                  return false;
               }
            } else if(!this$balance.equals(other$balance)) {
               return false;
            }

            Object this$bankAccountName = this.getBankAccountName();
            Object other$bankAccountName = other.getBankAccountName();
            if(this$bankAccountName == null) {
               if(other$bankAccountName != null) {
                  return false;
               }
            } else if(!this$bankAccountName.equals(other$bankAccountName)) {
               return false;
            }

            Object this$bankAccountNo = this.getBankAccountNo();
            Object other$bankAccountNo = other.getBankAccountNo();
            if(this$bankAccountNo == null) {
               if(other$bankAccountNo != null) {
                  return false;
               }
            } else if(!this$bankAccountNo.equals(other$bankAccountNo)) {
               return false;
            }

            Object this$bankName = this.getBankName();
            Object other$bankName = other.getBankName();
            if(this$bankName == null) {
               if(other$bankName != null) {
                  return false;
               }
            } else if(!this$bankName.equals(other$bankName)) {
               return false;
            }

            Object this$buyerName = this.getBuyerName();
            Object other$buyerName = other.getBuyerName();
            if(this$buyerName == null) {
               if(other$buyerName != null) {
                  return false;
               }
            } else if(!this$buyerName.equals(other$buyerName)) {
               return false;
            }

            Object this$buyerAccount = this.getBuyerAccount();
            Object other$buyerAccount = other.getBuyerAccount();
            if(this$buyerAccount == null) {
               if(other$buyerAccount != null) {
                  return false;
               }
            } else if(!this$buyerAccount.equals(other$buyerAccount)) {
               return false;
            }

            Object this$currency = this.getCurrency();
            Object other$currency = other.getCurrency();
            if(this$currency == null) {
               if(other$currency != null) {
                  return false;
               }
            } else if(!this$currency.equals(other$currency)) {
               return false;
            }

            Object this$depositBankNo = this.getDepositBankNo();
            Object other$depositBankNo = other.getDepositBankNo();
            if(this$depositBankNo == null) {
               if(other$depositBankNo != null) {
                  return false;
               }
            } else if(!this$depositBankNo.equals(other$depositBankNo)) {
               return false;
            }

            Object this$goodsTitle = this.getGoodsTitle();
            Object other$goodsTitle = other.getGoodsTitle();
            if(this$goodsTitle == null) {
               if(other$goodsTitle != null) {
                  return false;
               }
            } else if(!this$goodsTitle.equals(other$goodsTitle)) {
               return false;
            }

            Object this$income = this.getIncome();
            Object other$income = other.getIncome();
            if(this$income == null) {
               if(other$income != null) {
                  return false;
               }
            } else if(!this$income.equals(other$income)) {
               return false;
            }

            Object this$iwAccountLogId = this.getIwAccountLogId();
            Object other$iwAccountLogId = other.getIwAccountLogId();
            if(this$iwAccountLogId == null) {
               if(other$iwAccountLogId != null) {
                  return false;
               }
            } else if(!this$iwAccountLogId.equals(other$iwAccountLogId)) {
               return false;
            }

            Object this$memo = this.getMemo();
            Object other$memo = other.getMemo();
            if(this$memo == null) {
               if(other$memo != null) {
                  return false;
               }
            } else if(!this$memo.equals(other$memo)) {
               return false;
            }

            Object this$merchantOutOrderNo = this.getMerchantOutOrderNo();
            Object other$merchantOutOrderNo = other.getMerchantOutOrderNo();
            if(this$merchantOutOrderNo == null) {
               if(other$merchantOutOrderNo != null) {
                  return false;
               }
            } else if(!this$merchantOutOrderNo.equals(other$merchantOutOrderNo)) {
               return false;
            }

            Object this$otherAccountEmail = this.getOtherAccountEmail();
            Object other$otherAccountEmail = other.getOtherAccountEmail();
            if(this$otherAccountEmail == null) {
               if(other$otherAccountEmail != null) {
                  return false;
               }
            } else if(!this$otherAccountEmail.equals(other$otherAccountEmail)) {
               return false;
            }

            Object this$otherAccountFullname = this.getOtherAccountFullname();
            Object other$otherAccountFullname = other.getOtherAccountFullname();
            if(this$otherAccountFullname == null) {
               if(other$otherAccountFullname != null) {
                  return false;
               }
            } else if(!this$otherAccountFullname.equals(other$otherAccountFullname)) {
               return false;
            }

            Object this$otherUserId = this.getOtherUserId();
            Object other$otherUserId = other.getOtherUserId();
            if(this$otherUserId == null) {
               if(other$otherUserId != null) {
                  return false;
               }
            } else if(!this$otherUserId.equals(other$otherUserId)) {
               return false;
            }

            Object this$outcome = this.getOutcome();
            Object other$outcome = other.getOutcome();
            if(this$outcome == null) {
               if(other$outcome != null) {
                  return false;
               }
            } else if(!this$outcome.equals(other$outcome)) {
               return false;
            }

            Object this$partnerId = this.getPartnerId();
            Object other$partnerId = other.getPartnerId();
            if(this$partnerId == null) {
               if(other$partnerId != null) {
                  return false;
               }
            } else if(!this$partnerId.equals(other$partnerId)) {
               return false;
            }

            Object this$sellerAccount = this.getSellerAccount();
            Object other$sellerAccount = other.getSellerAccount();
            if(this$sellerAccount == null) {
               if(other$sellerAccount != null) {
                  return false;
               }
            } else if(!this$sellerAccount.equals(other$sellerAccount)) {
               return false;
            }

            Object this$sellerFullname = this.getSellerFullname();
            Object other$sellerFullname = other.getSellerFullname();
            if(this$sellerFullname == null) {
               if(other$sellerFullname != null) {
                  return false;
               }
            } else if(!this$sellerFullname.equals(other$sellerFullname)) {
               return false;
            }

            Object this$serviceFee = this.getServiceFee();
            Object other$serviceFee = other.getServiceFee();
            if(this$serviceFee == null) {
               if(other$serviceFee != null) {
                  return false;
               }
            } else if(!this$serviceFee.equals(other$serviceFee)) {
               return false;
            }

            Object this$serviceFeeRatio = this.getServiceFeeRatio();
            Object other$serviceFeeRatio = other.getServiceFeeRatio();
            if(this$serviceFeeRatio == null) {
               if(other$serviceFeeRatio != null) {
                  return false;
               }
            } else if(!this$serviceFeeRatio.equals(other$serviceFeeRatio)) {
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

            Object this$tradeNo = this.getTradeNo();
            Object other$tradeNo = other.getTradeNo();
            if(this$tradeNo == null) {
               if(other$tradeNo != null) {
                  return false;
               }
            } else if(!this$tradeNo.equals(other$tradeNo)) {
               return false;
            }

            Object this$tradeRefundAmount = this.getTradeRefundAmount();
            Object other$tradeRefundAmount = other.getTradeRefundAmount();
            if(this$tradeRefundAmount == null) {
               if(other$tradeRefundAmount != null) {
                  return false;
               }
            } else if(!this$tradeRefundAmount.equals(other$tradeRefundAmount)) {
               return false;
            }

            Object this$transAccount = this.getTransAccount();
            Object other$transAccount = other.getTransAccount();
            if(this$transAccount == null) {
               if(other$transAccount != null) {
                  return false;
               }
            } else if(!this$transAccount.equals(other$transAccount)) {
               return false;
            }

            Object this$transCodeMsg = this.getTransCodeMsg();
            Object other$transCodeMsg = other.getTransCodeMsg();
            if(this$transCodeMsg == null) {
               if(other$transCodeMsg != null) {
                  return false;
               }
            } else if(!this$transCodeMsg.equals(other$transCodeMsg)) {
               return false;
            }

            Object this$transDate = this.getTransDate();
            Object other$transDate = other.getTransDate();
            if(this$transDate == null) {
               if(other$transDate != null) {
                  return false;
               }
            } else if(!this$transDate.equals(other$transDate)) {
               return false;
            }

            Object this$transOutOrderNo = this.getTransOutOrderNo();
            Object other$transOutOrderNo = other.getTransOutOrderNo();
            if(this$transOutOrderNo == null) {
               if(other$transOutOrderNo != null) {
                  return false;
               }
            } else if(!this$transOutOrderNo.equals(other$transOutOrderNo)) {
               return false;
            }

            Object this$subTransCodeMsg = this.getSubTransCodeMsg();
            Object other$subTransCodeMsg = other.getSubTransCodeMsg();
            if(this$subTransCodeMsg == null) {
               if(other$subTransCodeMsg != null) {
                  return false;
               }
            } else if(!this$subTransCodeMsg.equals(other$subTransCodeMsg)) {
               return false;
            }

            Object this$signProductName = this.getSignProductName();
            Object other$signProductName = other.getSignProductName();
            if(this$signProductName == null) {
               if(other$signProductName != null) {
                  return false;
               }
            } else if(!this$signProductName.equals(other$signProductName)) {
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

            Object this$tradeAt = this.getTradeAt();
            Object other$tradeAt = other.getTradeAt();
            if(this$tradeAt == null) {
               if(other$tradeAt != null) {
                  return false;
               }
            } else if(!this$tradeAt.equals(other$tradeAt)) {
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
      return other instanceof AlipayTrans;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $balance = this.getBalance();
      result = result * 31 + ($balance == null?0:$balance.hashCode());
      Object $bankAccountName = this.getBankAccountName();
      result = result * 31 + ($bankAccountName == null?0:$bankAccountName.hashCode());
      Object $bankAccountNo = this.getBankAccountNo();
      result = result * 31 + ($bankAccountNo == null?0:$bankAccountNo.hashCode());
      Object $bankName = this.getBankName();
      result = result * 31 + ($bankName == null?0:$bankName.hashCode());
      Object $buyerName = this.getBuyerName();
      result = result * 31 + ($buyerName == null?0:$buyerName.hashCode());
      Object $buyerAccount = this.getBuyerAccount();
      result = result * 31 + ($buyerAccount == null?0:$buyerAccount.hashCode());
      Object $currency = this.getCurrency();
      result = result * 31 + ($currency == null?0:$currency.hashCode());
      Object $depositBankNo = this.getDepositBankNo();
      result = result * 31 + ($depositBankNo == null?0:$depositBankNo.hashCode());
      Object $goodsTitle = this.getGoodsTitle();
      result = result * 31 + ($goodsTitle == null?0:$goodsTitle.hashCode());
      Object $income = this.getIncome();
      result = result * 31 + ($income == null?0:$income.hashCode());
      Object $iwAccountLogId = this.getIwAccountLogId();
      result = result * 31 + ($iwAccountLogId == null?0:$iwAccountLogId.hashCode());
      Object $memo = this.getMemo();
      result = result * 31 + ($memo == null?0:$memo.hashCode());
      Object $merchantOutOrderNo = this.getMerchantOutOrderNo();
      result = result * 31 + ($merchantOutOrderNo == null?0:$merchantOutOrderNo.hashCode());
      Object $otherAccountEmail = this.getOtherAccountEmail();
      result = result * 31 + ($otherAccountEmail == null?0:$otherAccountEmail.hashCode());
      Object $otherAccountFullname = this.getOtherAccountFullname();
      result = result * 31 + ($otherAccountFullname == null?0:$otherAccountFullname.hashCode());
      Object $otherUserId = this.getOtherUserId();
      result = result * 31 + ($otherUserId == null?0:$otherUserId.hashCode());
      Object $outcome = this.getOutcome();
      result = result * 31 + ($outcome == null?0:$outcome.hashCode());
      Object $partnerId = this.getPartnerId();
      result = result * 31 + ($partnerId == null?0:$partnerId.hashCode());
      Object $sellerAccount = this.getSellerAccount();
      result = result * 31 + ($sellerAccount == null?0:$sellerAccount.hashCode());
      Object $sellerFullname = this.getSellerFullname();
      result = result * 31 + ($sellerFullname == null?0:$sellerFullname.hashCode());
      Object $serviceFee = this.getServiceFee();
      result = result * 31 + ($serviceFee == null?0:$serviceFee.hashCode());
      Object $serviceFeeRatio = this.getServiceFeeRatio();
      result = result * 31 + ($serviceFeeRatio == null?0:$serviceFeeRatio.hashCode());
      Object $totalFee = this.getTotalFee();
      result = result * 31 + ($totalFee == null?0:$totalFee.hashCode());
      Object $tradeNo = this.getTradeNo();
      result = result * 31 + ($tradeNo == null?0:$tradeNo.hashCode());
      Object $tradeRefundAmount = this.getTradeRefundAmount();
      result = result * 31 + ($tradeRefundAmount == null?0:$tradeRefundAmount.hashCode());
      Object $transAccount = this.getTransAccount();
      result = result * 31 + ($transAccount == null?0:$transAccount.hashCode());
      Object $transCodeMsg = this.getTransCodeMsg();
      result = result * 31 + ($transCodeMsg == null?0:$transCodeMsg.hashCode());
      Object $transDate = this.getTransDate();
      result = result * 31 + ($transDate == null?0:$transDate.hashCode());
      Object $transOutOrderNo = this.getTransOutOrderNo();
      result = result * 31 + ($transOutOrderNo == null?0:$transOutOrderNo.hashCode());
      Object $subTransCodeMsg = this.getSubTransCodeMsg();
      result = result * 31 + ($subTransCodeMsg == null?0:$subTransCodeMsg.hashCode());
      Object $signProductName = this.getSignProductName();
      result = result * 31 + ($signProductName == null?0:$signProductName.hashCode());
      Object $rate = this.getRate();
      result = result * 31 + ($rate == null?0:$rate.hashCode());
      Object $tradeAt = this.getTradeAt();
      result = result * 31 + ($tradeAt == null?0:$tradeAt.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "AlipayTrans(id=" + this.getId() + ", balance=" + this.getBalance() + ", bankAccountName=" + this.getBankAccountName() + ", bankAccountNo=" + this.getBankAccountNo() + ", bankName=" + this.getBankName() + ", buyerName=" + this.getBuyerName() + ", buyerAccount=" + this.getBuyerAccount() + ", currency=" + this.getCurrency() + ", depositBankNo=" + this.getDepositBankNo() + ", goodsTitle=" + this.getGoodsTitle() + ", income=" + this.getIncome() + ", iwAccountLogId=" + this.getIwAccountLogId() + ", memo=" + this.getMemo() + ", merchantOutOrderNo=" + this.getMerchantOutOrderNo() + ", otherAccountEmail=" + this.getOtherAccountEmail() + ", otherAccountFullname=" + this.getOtherAccountFullname() + ", otherUserId=" + this.getOtherUserId() + ", outcome=" + this.getOutcome() + ", partnerId=" + this.getPartnerId() + ", sellerAccount=" + this.getSellerAccount() + ", sellerFullname=" + this.getSellerFullname() + ", serviceFee=" + this.getServiceFee() + ", serviceFeeRatio=" + this.getServiceFeeRatio() + ", totalFee=" + this.getTotalFee() + ", tradeNo=" + this.getTradeNo() + ", tradeRefundAmount=" + this.getTradeRefundAmount() + ", transAccount=" + this.getTransAccount() + ", transCodeMsg=" + this.getTransCodeMsg() + ", transDate=" + this.getTransDate() + ", transOutOrderNo=" + this.getTransOutOrderNo() + ", subTransCodeMsg=" + this.getSubTransCodeMsg() + ", signProductName=" + this.getSignProductName() + ", rate=" + this.getRate() + ", tradeAt=" + this.getTradeAt() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
