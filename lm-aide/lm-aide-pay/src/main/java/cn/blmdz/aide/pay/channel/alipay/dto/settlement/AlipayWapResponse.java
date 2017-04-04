package cn.blmdz.aide.pay.channel.alipay.dto.settlement;

import com.thoughtworks.xstream.annotations.XStreamAlias;

import lombok.Data;

@Data
@XStreamAlias("notify")
public class AlipayWapResponse {
	@XStreamAlias("notify_time")
	private String notifyTime;
	@XStreamAlias("notify_type")
	private String notifyType;
	@XStreamAlias("notify_id")
	private String notifyId;
	@XStreamAlias("sign_type")
	private String signType;
	@XStreamAlias("sign")
	private String sign;
	@XStreamAlias("out_trade_no")
	private String outTradeNo;
	@XStreamAlias("subject")
	private String subject;
	@XStreamAlias("payment_type")
	private String paymentType;
	@XStreamAlias("trade_no")
	private String tradeNo;
	@XStreamAlias("trade_status")
	private String tradeStatus;
	@XStreamAlias("gmt_create")
	private String gmtCreate;
	@XStreamAlias("gmt_payment")
	private String gmtPayment;
	@XStreamAlias("gmt_close")
	private String gmtClose;
	@XStreamAlias("refund_status")
	private String refundStatus;
	@XStreamAlias("gmt_refund")
	private String gmtRefund;
	@XStreamAlias("seller_email")
	private String sellerEmail;
	@XStreamAlias("buyer_email")
	private String buyerEmail;
	@XStreamAlias("seller_id")
	private String sellerId;
	@XStreamAlias("buyer_id")
	private String buyerId;
	@XStreamAlias("price")
	private Double price;
	@XStreamAlias("total_fee")
	private Double totalFee;
	@XStreamAlias("quantity")
	private Integer quantity;
	@XStreamAlias("body")
	private String body;
	@XStreamAlias("discount")
	private Double discount;
	@XStreamAlias("is_total_fee_adjust")
	private String isTotalFeeAdjust;
	@XStreamAlias("use_coupon")
	private String useCoupon;
	@XStreamAlias("extra_common_param")
	private String extraCommonParam;
	@XStreamAlias("out_channel_type")
	private String outChannelType;
	@XStreamAlias("out_channel_amount")
	private String outChannelAmount;
	@XStreamAlias("out_channel_inst")
	private String outChannelInst;
	@XStreamAlias("business_scene")
	private String businessScene;
}
