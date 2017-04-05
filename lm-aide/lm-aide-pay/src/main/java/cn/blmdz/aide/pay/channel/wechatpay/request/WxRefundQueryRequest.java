package cn.blmdz.aide.pay.channel.wechatpay.request;

import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Preconditions;
import com.google.common.collect.Lists;

import cn.blmdz.aide.pay.channel.wechatpay.dto.WxPayRefundDto;
import cn.blmdz.aide.pay.channel.wechatpay.dto.WxPayRefundQueryResponse;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;

public class WxRefundQueryRequest extends WxRequest {
	private static final Logger log = LoggerFactory.getLogger(WxRefundQueryRequest.class);

	protected WxRefundQueryRequest(WxToken config) {
		super(config);
	}

	public static WxRefundQueryRequest build(WxToken config) {
		return new WxRefundQueryRequest(config);
	}

	public WxRefundQueryRequest transactionId(String transactionId) {
		Preconditions.checkArgument(Arguments.notEmpty(transactionId), "wechatpay.transactionId.empty");
		this.params.put("transaction_id", transactionId);
		return this;
	}

	public WxRefundQueryRequest outTradeNo(String outTradeNo) {
		Preconditions.checkArgument(Arguments.notEmpty(outTradeNo), "wechatpay.outTradeNo.empty");
		this.params.put("out_trade_no", outTradeNo);
		return this;
	}

	public WxRefundQueryRequest outRefundNo(String outRefundNo) {
		Preconditions.checkArgument(Arguments.notEmpty(outRefundNo), "wechatpay.outRefundNo.empty");
		this.params.put("out_refund_no", outRefundNo);
		return this;
	}

	public WxRefundQueryRequest refundId(String refundId) {
		Preconditions.checkArgument(Arguments.notEmpty(refundId), "wechatpay.refundId.empty");
		this.params.put("refund_id", refundId);
		return this;
	}

	@SuppressWarnings("unchecked")
	public Response<WxPayRefundQueryResponse> query() {
		Response<WxPayRefundQueryResponse> result = new Response<WxPayRefundQueryResponse>();
		super.nonceStr();
		super.sign();
		String preXml = toXml(this.params);
		log.debug("wx refund query url: {}", this.config.getQueryRefundGateway());
		log.debug("wx refund query xml: {}", preXml);
		String body = HttpRequest.post(this.config.getQueryRefundGateway()).send(preXml).trustAllCerts().trustAllHosts()
				.body();
		log.debug("wx refund query body: {}", body);
		if (verify(body, this.config)) {
			WxPayRefundQueryResponse refundQueryResponse = (WxPayRefundQueryResponse) parse(body,
					WxPayRefundQueryResponse.class);
			if (refundQueryResponse.isSuccess()) {
				refundQueryResponse.setWxPayRefundDtos(
						this.paresFromMap((Map<String, Object>)fromXML(body), Integer.valueOf(refundQueryResponse.getRefundCount())));
			}

			result.setResult(refundQueryResponse);
		}

		return result;
	}

	private List<WxPayRefundDto> paresFromMap(Map<String, Object> map, Integer num) {
		List<WxPayRefundDto> list = Lists.newArrayList();

		for (int i = 0; i < num.intValue(); ++i) {
			WxPayRefundDto payRefundPer = new WxPayRefundDto();
			payRefundPer.setOutRefundNo(this.parseToString(map.get("out_refund_no_" + i)));
			payRefundPer.setRefundId(this.parseToString(map.get("refund_id_" + i)));
			payRefundPer.setRefundChannel(this.parseToString(map.get("refund_channel_" + i)));
			payRefundPer.setRefundFee(this.parseToString(map.get("refund_fee_" + i)));
			payRefundPer.setCouponFefundFee(this.parseToString(map.get("coupon_refund_fee_" + i)));
			payRefundPer.setRefundStatus(this.parseToString(map.get("refund_status_" + i)));
			list.add(payRefundPer);
		}

		return list;
	}

	private String parseToString(Object o) {
		return o == null ? "" : o.toString();
	}
}
