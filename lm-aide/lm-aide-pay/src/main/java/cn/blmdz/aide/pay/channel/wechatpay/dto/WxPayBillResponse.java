package cn.blmdz.aide.pay.channel.wechatpay.dto;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@ToString
@Getter
@Setter
public class WxPayBillResponse {
	private String totalCount;
	private String totalFee;
	private String totalRefundFee;
	private String totalCouponRefundFee;
	private String totalPoundageFee;
	private String sign;
	private List<WxPayBillDto> wxPayBillDtos;

	public static WxPayBillResponse newInstance(List<String> cols) {
		WxPayBillResponse response = new WxPayBillResponse();
		response.setTotalCount(cols.get(0));
		response.setTotalFee(cols.get(1));
		response.setTotalRefundFee(cols.get(2));
		response.setTotalCouponRefundFee(cols.get(3));
		response.setTotalPoundageFee(cols.get(4));
		return response;
	}
}
