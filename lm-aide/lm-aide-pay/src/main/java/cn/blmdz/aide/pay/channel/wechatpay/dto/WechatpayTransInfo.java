package cn.blmdz.aide.pay.channel.wechatpay.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import lombok.Data;

@Data
public class WechatpayTransInfo implements Serializable {
	private static final long serialVersionUID = 1L;
	private static final BigDecimal ratio = new BigDecimal("100");
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

	public Long refundFeeToFen() {
		if (this.refundFee == null) {
			return Long.valueOf(0L);
		} else {
			BigDecimal money = new BigDecimal(this.refundFee);
			return Long.valueOf(money.multiply(ratio).longValue());
		}
	}

	public Long totalFeeToFen() {
		if (this.totalFee == null) {
			return Long.valueOf(0L);
		} else {
			BigDecimal money = new BigDecimal(this.totalFee);
			return Long.valueOf(money.multiply(ratio).longValue());
		}
	}

	public Long poundageFeeToFen() {
		if (this.poundageFee == null) {
			return Long.valueOf(0L);
		} else {
			BigDecimal money = new BigDecimal(this.poundageFee);
			return Long.valueOf(money.multiply(ratio).longValue());
		}
	}
}
