package cn.blmdz.aide.pay.utils;

import com.google.common.base.Strings;

import cn.blmdz.aide.pay.exception.PayRequestParamException;
import cn.blmdz.home.common.util.Arguments;

public class PayParamValid {
	public static void validPayParam(String channel, String subject, String content, String tradeNo, Integer fee) {
		if (Strings.isNullOrEmpty(tradeNo)) {
			throw new PayRequestParamException("pay.request.param.trade.no.invalid");
		} else if (Arguments.isNull(fee)) {
			throw new PayRequestParamException("pay.request.param.fee.invalid");
		} else if (Strings.isNullOrEmpty(channel)) {
			throw new PayRequestParamException("pay.request.param.channel.invalid");
		} else if (Strings.isNullOrEmpty(subject)) {
			throw new PayRequestParamException("pay.request.param.subject.invalid");
		} else if (Strings.isNullOrEmpty(content)) {
			throw new PayRequestParamException("pay.request.param.content.invalid");
		}
	}

	public static void validRefundParam(Long refundTrackId, String channel, String tradeNo, String refundNo,
			String paymentCode, Integer refundAmount) {
		if (Strings.isNullOrEmpty(tradeNo)) {
			throw new PayRequestParamException("refund.request.param.trade.no.invalid");
		} else if (Strings.isNullOrEmpty(refundNo)) {
			throw new PayRequestParamException("refund.request.param.refund.no.invalid");
		} else if (Strings.isNullOrEmpty(paymentCode)) {
			throw new PayRequestParamException("refund.request.param.payment.code.invalid");
		} else if (Arguments.isNull(refundAmount)) {
			throw new PayRequestParamException("refund.request.param.fee.invalid");
		} else if (Strings.isNullOrEmpty(channel)) {
			throw new PayRequestParamException("refund.request.param.channel.invalid");
		} else if (Arguments.isNull(refundTrackId)) {
			throw new PayRequestParamException("refund.request.param.track.id.invalid");
		}
	}
}
