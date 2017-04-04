package cn.blmdz.aide.pay.channel.alipay.dto;

import java.util.List;

import com.google.common.base.Strings;
import com.google.common.collect.Lists;

import cn.blmdz.aide.pay.common.Refund;
import lombok.EqualsAndHashCode;
import lombok.Getter;

@EqualsAndHashCode(callSuper = false)
public class AlipayRefundContext extends Refund {
	@Getter
	private List<AlipayRefundData> alipayRefundDatas;

	public AlipayRefundContext(Long refundTrackId, String payChannel) {
		super(refundTrackId, payChannel);
	}

	public AlipayRefundContext appendRefundDetail(String tradeNo, Integer refundAmount, String reason) {
		if (this.alipayRefundDatas == null) {
			this.alipayRefundDatas = Lists.newArrayList();
		}

		this.alipayRefundDatas
				.add(new AlipayRefundData(tradeNo, refundAmount, Strings.isNullOrEmpty(reason) ? "退款" : reason));
		return this;
	}

}
