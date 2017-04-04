package cn.blmdz.aide.pay.channel.alipay.dto.settlement;

import com.thoughtworks.xstream.annotations.XStreamAlias;

import lombok.Data;

@Data
@XStreamAlias("AccountQueryAccountLogVO")
public class AlipaySettlementDto {
	@XStreamAlias("balance")
	protected String balance;
	@XStreamAlias("bank_account_name")
	protected String bankAccountName;
	@XStreamAlias("bank_account_no")
	protected String bankAccountNo;
	@XStreamAlias("bank_name")
	protected String bankName;
	@XStreamAlias("buyer_name")
	protected String buyerName;
	@XStreamAlias("buyer_account")
	protected String buyerAccount;
	@XStreamAlias("currency")
	protected String currency;
	@XStreamAlias("deposit_bank_no")
	protected String depositBankNo;
	@XStreamAlias("goods_title")
	protected String goodsTitle;
	@XStreamAlias("income")
	protected String income;
	@XStreamAlias("iw_account_log_id")
	protected String iwAccountLogId;
	@XStreamAlias("memo")
	protected String memo;
	@XStreamAlias("merchant_out_order_no")
	protected String merchantOutOrderNo;
	@XStreamAlias("other_account_email")
	protected String otherAccountEmail;
	@XStreamAlias("other_account_fullname")
	protected String otherAccountFullname;
	@XStreamAlias("other_user_id")
	protected String otherUserId;
	@XStreamAlias("outcome")
	protected String outcome;
	@XStreamAlias("partner_id")
	protected String partnerId;
	@XStreamAlias("seller_account")
	protected String sellerAccount;
	@XStreamAlias("seller_fullname")
	protected String sellerFullname;
	@XStreamAlias("service_fee")
	protected String serviceFee;
	@XStreamAlias("service_fee_ratio")
	protected String serviceFeeRatio;
	@XStreamAlias("total_fee")
	protected String totalFee;
	@XStreamAlias("trade_no")
	protected String tradeNo;
	@XStreamAlias("trade_refund_amount")
	protected String tradeRefundAmount;
	@XStreamAlias("trans_account")
	protected String transAccount;
	@XStreamAlias("trans_code_msg")
	protected String transCodeMsg;
	@XStreamAlias("trans_date")
	protected String transDate;
	@XStreamAlias("trans_out_order_no")
	protected String transOutOrderNo;
	@XStreamAlias("sub_trans_code_msg")
	protected String subTransCodeMsg;
	@XStreamAlias("sign_product_name")
	protected String signProductName;
	@XStreamAlias("rate")
	protected String rate;
}
