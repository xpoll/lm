package cn.blmdz.aide.pay.channel.wechatpay.manager;

import java.util.Date;

import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;

import cn.blmdz.aide.pay.channel.alipay.dto.RedirectInfo;
import cn.blmdz.aide.pay.channel.wechatpay.OpenIdUtil;
import cn.blmdz.aide.pay.channel.wechatpay.dto.WxPrePayResponse;
import cn.blmdz.aide.pay.channel.wechatpay.request.WxDownloadBillRequest;
import cn.blmdz.aide.pay.channel.wechatpay.request.WxJsApiRequest;
import cn.blmdz.aide.pay.channel.wechatpay.request.WxPrePayRequest;
import cn.blmdz.aide.pay.channel.wechatpay.request.WxRefundRequest;
import cn.blmdz.aide.pay.channel.wechatpay.request.WxToken;
import cn.blmdz.aide.pay.enums.TradeType;
import cn.blmdz.aide.pay.exception.PayRequestParamException;
import cn.blmdz.aide.pay.utils.PayParamValid;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.home.common.util.MapBuilder;
import cn.blmdz.home.common.util.NumberUtils;

public class WechatpayManager {
	private static final Logger log = LoggerFactory.getLogger(WechatpayManager.class);
	// private DateTimeFormatter STD_FMT = DateTimeFormat.forPattern("YYYY-MM-dd HH:mm:ss");

	public RedirectInfo prePay(String channel, String subject, String content, String tradeNo, Integer fee,
			String notifyUrl, Long buyerId, WxToken wxToken) {
		RedirectInfo info = new RedirectInfo();

		try {
			PayParamValid.validPayParam(channel, subject, content, tradeNo, fee);
			this.checkParam(wxToken, notifyUrl, Integer.valueOf(TradeType.PAY.value()));
			if (channel.endsWith("jsapi")) {
				String xml = WxPrePayRequest.build(wxToken).notifyUrl(notifyUrl)
						.body(subject.substring(0, Math.min(50, subject.length()))).outTradeNo(tradeNo).totalFee(fee)
						.ip("127.0.0.1").tradeType(WxPrePayRequest.TradeType.JSAPI)
						.openId(OpenIdUtil.getOpenId(buyerId)).prePayParams(wxToken.getGateway());
				info.setResult(xml);
			} else {
				String xml = WxPrePayRequest.build(wxToken).notifyUrl(notifyUrl)
						.body(subject.substring(0, Math.min(50, subject.length()))).outTradeNo(tradeNo).totalFee(fee)
						.productId("1").timeStart(DateTime.now().toDate())
						.timeExpire(DateTime.now().plusDays(1).toDate()).attach("attach")
						.tradeType(WxPrePayRequest.TradeType.NATIVE).prePayParams(wxToken.getGateway());
				info.setResult(xml);
			}

			info.setChannel(channel);
			info.setIsRedirectNow(Boolean.TRUE);
		} catch (PayRequestParamException var11) {
			log.error("assembly prepay param fail,error: {}", var11.getMessage());
			info.setError(var11.getMessage());
		} catch (Exception var12) {
			log.error("assembly prepay param fail,cause: {}", Throwables.getStackTraceAsString(var12));
			info.setError("assembly.prepay.param.fail");
		}

		return info;
	}

	public RedirectInfo mockPay(String channel, String subject, String content, String tradeNo, Integer fee,
			String notifyUrl, Long buyerId, WxToken wxToken) {
		RedirectInfo info = new RedirectInfo();

		try {
			OpenIdUtil.putOpenId(buyerId, "osvRcuFMHhG-31pJuIVuyi6N0VbE");
			WxPrePayRequest request = WxPrePayRequest.build(wxToken).notifyUrl(notifyUrl)
					.body(subject.substring(0, Math.min(50, subject.length()))).outTradeNo(tradeNo).totalFee(fee)
					.ip("127.0.0.1").tradeType(WxPrePayRequest.TradeType.JSAPI).openId(OpenIdUtil.getOpenId(buyerId));
			request.param().put("total_fee", NumberUtils.formatPrice(fee));
			String res = request.mockUrl(wxToken.getGateway());
			info.setResult(res);
			info.setChannel(channel);
			info.setIsRedirectNow(Boolean.TRUE);
		} catch (PayRequestParamException var12) {
			log.error("assembly mockpar param fail,error: {}", var12.getMessage());
			info.setError(var12.getMessage());
		} catch (Exception var13) {
			log.error("assembly mockpar param fail,cause: {}", Throwables.getStackTraceAsString(var13));
			info.setError("assembly.mockpar.param.fail");
		}

		return info;
	}

	public RedirectInfo pay(String channel, String tradeNo, WxPrePayResponse prePayRes, WxToken wxToken) {
		return channel.endsWith("jsapi") ? this.handleJsapi(channel, prePayRes, wxToken)
				: this.handleNative(channel, tradeNo, prePayRes, wxToken);
	}

	public RedirectInfo refund(Long refundTrackId, String caFilePath, String certFilePath, String channel,
			String tradeNo, String refundNo, String paymentCode, Integer refundAmount, Integer totalFee,
			WxToken wxToken) {
		RedirectInfo info = new RedirectInfo();

		try {
			this.validRefundParam(refundTrackId, channel, tradeNo, refundNo, paymentCode, refundAmount);
			this.checkParam(wxToken, (String) null, Integer.valueOf(TradeType.REFUND.value()));
			String xml = WxRefundRequest.build(wxToken).caFilePath(caFilePath).certFilePath(certFilePath)
					.outTradeNo(tradeNo).transactionId(paymentCode).outRefundNo(refundNo).refundFee(refundAmount)
					.totalFee(totalFee).refundParam();
			info.setResult(xml);
			info.setIsRedirectNow(Boolean.FALSE);
			info.setChannel(channel);
		} catch (PayRequestParamException var13) {
			log.error("assembly wx refund request param fail,error: {}", var13.getMessage());
			info.setError(var13.getMessage());
		} catch (Exception var14) {
			log.error("assembly wx refund request param fail,cause: {}", Throwables.getStackTraceAsString(var14));
			info.setError("assembly.wx.refund.request.param.fail");
		}

		return info;
	}

	public RedirectInfo mockRefund(Long refundTrackId, String channel, String tradeNo, String refundNo,
			String paymentCode, Integer refundAmount, Integer totalFee, WxToken wxToken) {
		RedirectInfo info = new RedirectInfo();

		try {
			this.validRefundParam(refundTrackId, channel, tradeNo, refundNo, paymentCode, refundAmount);
			this.checkParam(wxToken, (String) null, Integer.valueOf(TradeType.REFUND.value()));
			String xml = WxRefundRequest.build(wxToken).outTradeNo(tradeNo).transactionId(paymentCode)
					.outRefundNo(refundNo).refundFee(refundAmount).totalFee(totalFee)
					.mockUrl(wxToken.getRefundGateway());
			info.setResult(xml);
			info.setIsRedirectNow(Boolean.TRUE);
			info.setChannel(channel);
		} catch (PayRequestParamException var11) {
			log.error("assembly wx mock refund request param fail,error: {}", var11.getMessage());
			info.setError(var11.getMessage());
		} catch (Exception var12) {
			log.error("assembly wx mock refund request param fail,cause: {}", Throwables.getStackTraceAsString(var12));
			info.setError("assembly.wx.refund.request.param.fail");
		}

		return info;
	}

	private void validRefundParam(Long refundTrackId, String channel, String tradeNo, String refundNo,
			String paymentCode, Integer refundAmount) {
		if (Strings.isNullOrEmpty(tradeNo)) {
			throw new PayRequestParamException("alipay.refund.request.param.trade.no.invalid");
		} else if (Strings.isNullOrEmpty(refundNo)) {
			throw new PayRequestParamException("alipay.refund.request.param.refund.no.invalid");
		} else if (Strings.isNullOrEmpty(paymentCode)) {
			throw new PayRequestParamException("alipay.refund.request.param.payment.code.invalid");
		} else if (Arguments.isNull(refundAmount)) {
			throw new PayRequestParamException("alipay.refund.request.param.fee.invalid");
		} else if (Strings.isNullOrEmpty(channel)) {
			throw new PayRequestParamException("alipay.refund.request.param.channel.invalid");
		} else if (Arguments.isNull(refundTrackId)) {
			throw new PayRequestParamException("alipay.refund.request.param.track.id.invalid");
		}
	}

	private RedirectInfo handleJsapi(String channel, WxPrePayResponse prePayRes, WxToken wxToken) {
		RedirectInfo info = new RedirectInfo();

		try {
			String prepayId = prePayRes.getPrepayId();
			info.setChannel(channel);
			info.setResult(WxJsApiRequest.build(wxToken).prepayId(prepayId).getJson());
			info.setIsRedirectNow(Boolean.FALSE);
		} catch (IllegalArgumentException var6) {
			log.error("handle js api fail where channel: {} WxPrePayResponse:{} ,error:{}",
					new Object[] { channel, prePayRes, var6.getMessage() });
			info.setError(var6.getMessage());
		} catch (Exception var7) {
			log.error("handle js api fail where channel: {} WxPrePayResponse:{} ,cause:{}",
					new Object[] { channel, prePayRes, Throwables.getStackTraceAsString(var7) });
			info.setError("handle.jsapi.fail");
		}

		return info;
	}

	private void checkParam(WxToken wxToken, String notifyUrl, Integer type) {
		if (Strings.isNullOrEmpty(wxToken.getAppId())) {
			throw new PayRequestParamException("wechat.pay.request.param.appId.invalid");
		} else if (Strings.isNullOrEmpty(wxToken.getSecret())) {
			throw new PayRequestParamException("wechat.pay.request.param.secret.invalid");
		} else if (Strings.isNullOrEmpty(wxToken.getMchId())) {
			throw new PayRequestParamException("wechat.pay.request.param.mchId.invalid");
		} else if (Strings.isNullOrEmpty(wxToken.getPaternerkey())) {
			throw new PayRequestParamException("wechat.pay.request.param.paternerkey.invalid");
		} else if (Strings.isNullOrEmpty(wxToken.getGateway())) {
			throw new PayRequestParamException("pay.request.param.gateway.invalid");
		} else if (type.equals(Integer.valueOf(TradeType.PAY.value())) && Strings.isNullOrEmpty(notifyUrl)) {
			throw new PayRequestParamException("pay.request.param.notify.url.invalid");
		} else {
			if (type.equals(Integer.valueOf(TradeType.REFUND.value()))) {
				;
			}

		}
	}

	private RedirectInfo handleNative(String channel, String tradeNo, WxPrePayResponse prePayRes, WxToken wxToken) {
		RedirectInfo info = new RedirectInfo();

		try {
			String codeUrl = prePayRes.getCodeUrl();
			info.setIsRedirectNow(Boolean.FALSE);
			info.setResult(JsonMapper.nonEmptyMapper()
					.toJson(MapBuilder.of().put("qrcodeUrl", codeUrl, "tradeNo", tradeNo).map()));
			info.setChannel(channel);
		} catch (Exception var7) {
			log.error("handle native where channel:{} tradeNo: {} WxPrePayResponse: {} fail,cause:{}",
					new Object[] { channel, tradeNo, prePayRes, Throwables.getStackTraceAsString(var7) });
			info.setError("handle.native.fail");
		}

		return info;
	}

	public String getInnerChannel() {
		return "wechatpay";
	}

	public boolean match(String channel) {
		return !Strings.isNullOrEmpty(channel)
				? (channel.contains("wechatpay") ? Boolean.TRUE.booleanValue() : Boolean.FALSE.booleanValue())
				: Boolean.FALSE.booleanValue();
	}

	public RedirectInfo syncThridTrans(Date billDate, WxDownloadBillRequest.BillType billType, WxToken wxToken) {
		RedirectInfo info = new RedirectInfo();

		try {
			Preconditions.checkArgument(Arguments.notNull(billDate), "sysc.thrid.trans.context.billDate.is.null");
			Preconditions.checkArgument(Arguments.notNull(billType), "sysc.thrid.trans.context.billType.is.null");
			this.checkParam(wxToken, (String) null, Integer.valueOf(TradeType.SYNC_SETTLEMENT.value()));
			String xml = WxDownloadBillRequest.build(wxToken).billType(billType).billDate(billDate).queryParam();
			info.setResult(xml);
			info.setIsRedirectNow(Boolean.FALSE);
			info.setChannel("wechatpay");
		} catch (IllegalArgumentException var6) {
			log.error("Failed to assembly wechatpay bill request error:{}", var6.getMessage());
			info.setError(var6.getMessage());
		} catch (Exception var7) {
			log.error("Failed to assembly wechatpay bill request cause:{}", Throwables.getStackTraceAsString(var7));
			info.setError("failed.to.assembly.wechatpay.bill.request");
		}

		return info;
	}
}
