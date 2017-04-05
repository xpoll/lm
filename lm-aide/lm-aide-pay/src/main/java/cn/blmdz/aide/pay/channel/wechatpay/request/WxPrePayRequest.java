package cn.blmdz.aide.pay.channel.wechatpay.request;

import java.util.Date;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Joiner;
import com.google.common.base.Objects;
import com.google.common.base.Preconditions;

import cn.blmdz.aide.pay.channel.wechatpay.dto.WxPrePayResponse;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;

public class WxPrePayRequest extends WxRequest {
	private static final Logger log = LoggerFactory.getLogger(WxPrePayRequest.class);

	protected WxPrePayRequest(WxToken config) {
		super(config);
	}

	public static WxPrePayRequest build(WxToken config) {
		return new WxPrePayRequest(config);
	}

	public WxPrePayRequest notifyUrl(String notifyUrl) {
		Preconditions.checkArgument(Arguments.notEmpty(notifyUrl), "wechatpay.notifyUrl.empty");
		this.params.put("notify_url", notifyUrl);
		return this;
	}

	public WxPrePayRequest body(String body) {
		Preconditions.checkArgument(Arguments.notEmpty(body), "wechatpay.body.empty");
		this.params.put("body", body);
		return this;
	}

	public WxPrePayRequest outTradeNo(String outTradeNo) {
		Preconditions.checkArgument(Arguments.notEmpty(outTradeNo), "wechatpay.outTradeNo.empty");
		Preconditions.checkArgument(outTradeNo.length() <= 32, "wechatpay.outTradeNo.length.gt32");
		this.params.put("out_trade_no", outTradeNo);
		return this;
	}

	public WxPrePayRequest totalFee(Integer totalFee) {
		Preconditions.checkArgument(Arguments.notNull(totalFee), "wechatpay.totalFee.null");
		Preconditions.checkArgument(Arguments.positive(totalFee), "wechatpay.totalFee.not.positive");
		this.params.put("total_fee", totalFee);
		return this;
	}

	public WxPrePayRequest ip(String ip) {
		Preconditions.checkArgument(Arguments.notEmpty(ip), "wechatpay.outTradeNo.empty");
		this.params.put("spbill_create_ip", ip);
		return this;
	}

	public WxPrePayRequest tradeType(WxPrePayRequest.TradeType tradeType) {
		Preconditions.checkArgument(Arguments.notNull(tradeType), "wechatpay.tradeType.null");
		this.params.put("trade_type", tradeType.getTitle());
		return this;
	}

	public WxPrePayRequest openId(String openId) {
		if (Objects.equal(this.param().get("trade_type"), WxPrePayRequest.TradeType.JSAPI.getTitle())) {
			Preconditions.checkArgument(Arguments.notEmpty(openId), "wechatpay.openId.empty");
		}

		this.params.put("openid", openId);
		return this;
	}

	public WxPrePayRequest attach(String attach) {
		Preconditions.checkArgument(Arguments.notEmpty(attach), "wechatpay.attach.empty");
		this.params.put("attach", attach);
		return this;
	}

	public WxPrePayRequest timeStart(Date timeStart) {
		if (timeStart != null) {
			this.params.put("time_start", formatDate(timeStart));
		}

		return this;
	}

	public WxPrePayRequest timeExpire(Date timeExpire) {
		if (timeExpire != null) {
			this.params.put("time_expire", formatDate(timeExpire));
		}

		return this;
	}

	public WxPrePayRequest productId(String productId) {
		if (Objects.equal(this.param().get("trade_type"), WxPrePayRequest.TradeType.NATIVE.getTitle())) {
			Preconditions.checkArgument(Arguments.notEmpty(productId), "wechatpay.productId.empty");
		}

		this.params.put("product_id", productId);
		return this;
	}

	public Response<WxPrePayResponse> prePay(String gateway) {
		Response<WxPrePayResponse> result = new Response<WxPrePayResponse>();
		super.nonceStr();
		super.sign();
		String preXml = toXml(this.params);
		log.debug("wx prepay url: {}", gateway);
		log.debug("wx prepay pre xml: {}", preXml);
		String body = HttpRequest.post(gateway).send(preXml).trustAllCerts().trustAllHosts().body();
		log.debug("wx prepay body: {}", body);
		if (verify(body, this.config)) {
			WxPrePayResponse wxPrePayResponse = (WxPrePayResponse) parse(body, WxPrePayResponse.class);
			result.setResult(wxPrePayResponse);
		}

		return result;
	}

	public String prePayParams(String gateway) {
		super.nonceStr();
		super.sign();
		String preXml = toXml(this.params);
		log.debug("wx prepay url: {}", gateway);
		log.debug("wx prepay pre xml: {}", preXml);
		return preXml;
	}

	public Response<String> prepayId(String gateway) {
		Response<String> result = new Response<String>();
		Response<WxPrePayResponse> response = this.prePay(gateway);
		if (response.isSuccess()) {
			result.setResult(((WxPrePayResponse) response.getResult()).getPrepayId());
		} else {
			result.setError(response.getError());
		}

		return result;
	}

	public String mockUrl(String gateway) {
		super.nonceStr();
		super.sign();
		String suffix = Joiner.on('&').withKeyValueSeparator("=").join(this.params);
		return gateway + "?" + suffix;
	}

	public Response<String> codeUrl(String gateway) {
		Response<String> result = new Response<String>();
		Response<WxPrePayResponse> response = this.prePay(gateway);
		if (response.isSuccess()) {
			result.setResult(((WxPrePayResponse) response.getResult()).getCodeUrl());
		} else {
			result.setError(response.getError());
		}

		return result;
	}

	public static enum TradeType {
		JSAPI("JSAPI"), NATIVE("NATIVE"), APP("APP");

		private String title;

		private TradeType(String title) {
			this.title = title;
		}

		public static WxPrePayRequest.TradeType from(String value) {
			for (WxPrePayRequest.TradeType tradeType : values()) {
				if (Objects.equal(value, tradeType.title)) {
					return tradeType;
				}
			}

			return null;
		}

		public String getTitle() {
			return this.title;
		}
	}
}
