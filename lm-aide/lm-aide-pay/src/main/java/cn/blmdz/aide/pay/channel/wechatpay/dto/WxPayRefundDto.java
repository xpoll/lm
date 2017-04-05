package cn.blmdz.aide.pay.channel.wechatpay.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class WxPayRefundDto {
	private Integer id;
	private String outRefundNo;
	private String refundId;
	private String refundChannel;
	private String refundFee;
	private String couponFefundFee;
	private String refundStatus;
}
