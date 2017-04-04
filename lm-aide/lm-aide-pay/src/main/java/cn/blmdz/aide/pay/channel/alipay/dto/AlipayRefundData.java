package cn.blmdz.aide.pay.channel.alipay.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class AlipayRefundData {
	private String tradeNo;
	private Integer refundAmount;
	private String reason;
}
