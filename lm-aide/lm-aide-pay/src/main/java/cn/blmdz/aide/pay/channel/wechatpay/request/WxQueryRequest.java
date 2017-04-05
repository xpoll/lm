package cn.blmdz.aide.pay.channel.wechatpay.request;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.MoreObjects;
import com.google.common.base.Preconditions;

import cn.blmdz.aide.pay.channel.wechatpay.dto.WxPayNotifyDto;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;

public class WxQueryRequest extends WxRequest {
	private static final Logger log = LoggerFactory.getLogger(WxQueryRequest.class);
	private static final String SEARCH_ORDER_URL = "https://api.mch.weixin.qq.com/pay/orderquery";

	protected WxQueryRequest(WxToken config) {
		super(config);
	}

	public static WxQueryRequest build(WxToken config) {
		return new WxQueryRequest(config);
	}

	public WxQueryRequest transactionId(String transactionId) {
		this.params.put("transaction_id", transactionId);
		return this;
	}

	public WxQueryRequest outTradeNo(String outTradeNo) {
		this.params.put("out_trade_no", outTradeNo);
		return this;
	}

	public Response<WxPayNotifyDto> query() {
		Response<WxPayNotifyDto> result = new Response<WxPayNotifyDto>();
		Preconditions.checkArgument(
				!Arguments.isEmpty(MoreObjects.firstNonNull(this.params.get("out_trade_no"), "").toString())
						|| !Arguments
								.isEmpty(MoreObjects.firstNonNull(this.params.get("transaction_id"), "").toString()),
				"params.atleast.one");
		super.nonceStr();
		super.sign();
		String preXml = toXml(this.params);
		log.debug("wx order query url: {}", SEARCH_ORDER_URL);
		log.debug("wx order query xml: {}", preXml);
		String body = HttpRequest.post(SEARCH_ORDER_URL).send(preXml).trustAllCerts().trustAllHosts().body();
		log.debug("wx order query body: {}", body);
		if (verify(body, this.config)) {
			WxPayNotifyDto wxPayNotifyDto = (WxPayNotifyDto) parse(body, WxPayNotifyDto.class);
			result.setResult(wxPayNotifyDto);
		}

		return result;
	}
}
