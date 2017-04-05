package cn.blmdz.aide.pay.channel.wechatpay.request;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.base.Throwables;

import cn.blmdz.aide.pay.channel.wechatpay.HttpsUtil;
import cn.blmdz.aide.pay.channel.wechatpay.dto.WxPayRefundResponse;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;

public class WxRefundRequest extends WxRequest {
	private static final Logger log = LoggerFactory.getLogger(WxRefundRequest.class);
	private String caFilePath = "";
	private String certFilePath = "";

	protected WxRefundRequest(WxToken config) {
		super(config);
		this.params.put("op_user_id", config.getMchId());
	}

	public static WxRefundRequest build(WxToken config) {
		return new WxRefundRequest(config);
	}

	public WxRefundRequest transactionId(String transactionId) {
		Preconditions.checkArgument(Arguments.notEmpty(transactionId), "wechatpay.transactionId.empty");
		this.params.put("transaction_id", transactionId);
		return this;
	}

	public WxRefundRequest outTradeNo(String outTradeNo) {
		Preconditions.checkArgument(Arguments.notEmpty(outTradeNo), "wechatpay.outTradeNo.empty");
		this.params.put("out_trade_no", outTradeNo);
		return this;
	}

	public WxRefundRequest outRefundNo(String outRefundNo) {
		Preconditions.checkArgument(Arguments.notEmpty(outRefundNo), "wechatpay.outRefundNo.empty");
		this.params.put("out_refund_no", outRefundNo);
		return this;
	}

	public WxRefundRequest totalFee(Integer totalFee) {
		Preconditions.checkArgument(Arguments.positive(totalFee), "wechatpay.totalFee.not.positive");
		this.params.put("total_fee", totalFee);
		return this;
	}

	public WxRefundRequest refundFee(Integer refundFee) {
		Preconditions.checkArgument(Arguments.positive(refundFee), "wechatpay.refundFee.not.positive");
		this.params.put("refund_fee", refundFee);
		return this;
	}

	public WxRefundRequest caFilePath(String caFilePath) {
		this.caFilePath = caFilePath;
		return this;
	}

	public WxRefundRequest certFilePath(String certFilePath) {
		this.certFilePath = certFilePath;
		return this;
	}

	public Response<WxPayRefundResponse> refund() {
		Response<WxPayRefundResponse> result = new Response<WxPayRefundResponse>();
		super.nonceStr();
		super.sign();
		String preXml = toXml(this.params);
		log.debug("wx order refund xml: {}", preXml);

		try {
			String body = HttpsUtil.build(this.caFilePath, this.certFilePath)
					.callHttps(preXml, this.config.getMchId(), this.config.getRefundGateway()).getResContent();
			log.debug("wx order refund body: {}", body);
			WxPayRefundResponse payRefundReply = (WxPayRefundResponse) parse(body, WxPayRefundResponse.class);
			result.setResult(payRefundReply);
		} catch (Exception var5) {
			result.setError("wx.refund.fail");
			log.error("wx refund fail, cause: {}", Throwables.getStackTraceAsString(var5));
		}

		return result;
	}

	public String refundParam() {
		super.nonceStr();
		super.sign();
		String preXml = toXml(this.params);
		log.debug("wx order refund xml: {}", preXml);
		return preXml;
	}

	public String mockUrl(String gateway) {
		super.nonceStr();
		super.sign();
		String suffix = Joiner.on('&').withKeyValueSeparator("=").join(this.params);
		return gateway + "?" + suffix;
	}
}
