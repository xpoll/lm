package cn.blmdz.aide.pay.channel.kjtpay.manager;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Objects;
import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;

import cn.blmdz.aide.pay.channel.alipay.dto.RedirectInfo;
import cn.blmdz.aide.pay.channel.kjtpay.dto.KjtpayTransInfo;
import cn.blmdz.aide.pay.channel.kjtpay.request.KjtPageQueryRequest;
import cn.blmdz.aide.pay.channel.kjtpay.request.KjtPayRequest;
import cn.blmdz.aide.pay.channel.kjtpay.request.KjtRefundRequest;
import cn.blmdz.aide.pay.channel.kjtpay.request.KjtToken;
import cn.blmdz.aide.pay.enums.TradeType;
import cn.blmdz.aide.pay.exception.PayRequestParamException;
import cn.blmdz.aide.pay.utils.PayParamValid;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.home.common.util.NumberUtils;

public class KjtpayManager {
	private static final Logger log = LoggerFactory.getLogger(KjtpayManager.class);
	private static final String PC_SERVICE = "create_instant_pay";
	private static final String MOBILE_SERVICE = "create_instant_pay_mobile";
	private static final DateTimeFormatter NUM_FMT = DateTimeFormat.forPattern("YYYYMMddHHmmss");

	public RedirectInfo pay(String channel, String subject, String content, String tradeNo, String systemNo,
			Date expiredAt, Integer fee, String notifyUrl, String returnUrl, KjtToken kjtToken) {
		RedirectInfo info = new RedirectInfo();
		PayParamValid.validPayParam(channel, subject, content, tradeNo, fee);
		this.checkParam(notifyUrl, null, returnUrl, kjtToken, channel, Integer.valueOf(TradeType.PAY.value()));
		String url = "";

		try {
			if (Objects.equal("kjtpay", channel)) {
				KjtPayRequest kjtPayRequest = KjtPayRequest.build(kjtToken).service(PC_SERVICE).requestNo(tradeNo)
						.tradeList(this.getTradeList(tradeNo, subject, content, fee, channel, notifyUrl, kjtToken))
						.returnUrl(returnUrl + "?data=" + systemNo);
				url = kjtPayRequest.url();
			} else {
				KjtPayRequest kjtPayRequest = KjtPayRequest.build(kjtToken).service(MOBILE_SERVICE).requestNo(tradeNo)
						.tradeList(this.getTradeList(tradeNo, subject, content, fee, channel, notifyUrl, kjtToken))
						.returnUrl(returnUrl + "?data=" + systemNo);
				url = kjtPayRequest.url();
			}
		} catch (Exception var15) {
			log.error("generate kjtpay url fail cause:{}", Throwables.getStackTraceAsString(var15));
		}

		info.setIsRedirectNow(Boolean.TRUE);
		info.setResult(url);
		info.setChannel(channel);
		return info;
	}

	private String getTradeList(String tradeNo, String subject, String content, Integer fee, String channel,
			String notifyUrl, KjtToken token) {
		StringBuilder tradeList = new StringBuilder();
		tradeList.append(tradeNo);
		tradeList.append("~");
		tradeList.append(subject);
		tradeList.append("~");
		tradeList.append(NumberUtils.formatPrice(fee));
		tradeList.append("~1~");
		tradeList.append(NumberUtils.formatPrice(fee));
		tradeList.append("~");
		tradeList.append("~");
		tradeList.append(token.getAccount());
		tradeList.append("~1~");
		tradeList.append(tradeNo);
		tradeList.append("~");
		tradeList.append(content);
		tradeList.append("~");
		tradeList.append("~");
		tradeList.append("0");
		tradeList.append("~~~");
		tradeList.append("~");
		if (Objects.equal("kjtpay", channel)) {
			tradeList.append(notifyUrl);
		} else {
			tradeList.append(notifyUrl);
		}

		return tradeList.toString().replaceAll("%", "").replaceAll(" ", "");
	}

	private void checkParam(String notifyUrl, String refundNotifyUrl, String returnUrl, KjtToken kjtToken,
			String channel, Integer type) {
		new KjtToken();
		if (Strings.isNullOrEmpty(kjtToken.getPid())) {
			throw new PayRequestParamException("kjt.pay.request.param.pid.invalid");
		} else if (Strings.isNullOrEmpty(kjtToken.getPfxPath())) {
			throw new PayRequestParamException("kjt.pay.request.param.pfxPath.invalid");
		} else if (Strings.isNullOrEmpty(kjtToken.getKeyPassword())) {
			throw new PayRequestParamException("kjt.pay.request.param.keyPassword.invalid");
		} else if (Strings.isNullOrEmpty(kjtToken.getCerPath())) {
			throw new PayRequestParamException("kjt.pay.request.param.cerPath.invalid");
		} else if (Strings.isNullOrEmpty(kjtToken.getAccount())) {
			throw new PayRequestParamException("kjt.pay.request.param.account.invalid");
		} else {
			if (type.equals(Integer.valueOf(TradeType.PAY.value()))) {
				if (channel.equals("kjtpay")) {
					if (Strings.isNullOrEmpty(notifyUrl)) {
						throw new PayRequestParamException("kjt.pay.request.param.notify.url.invalid");
					}

					if (Strings.isNullOrEmpty(returnUrl)) {
						throw new PayRequestParamException("kjt.pay.request.param.return.url.invalid");
					}
				}

				if (channel.equals("kjtpay-mobile")) {
					if (Strings.isNullOrEmpty(notifyUrl)) {
						throw new PayRequestParamException("kjt.pay.request.param.notify.url.invalid");
					}

					if (Strings.isNullOrEmpty(returnUrl)) {
						throw new PayRequestParamException("kjt.pay.request.param.return.url.invalid");
					}
				}
			}

			if (type.equals(Integer.valueOf(TradeType.REFUND.value())) && Strings.isNullOrEmpty(refundNotifyUrl)) {
				throw new PayRequestParamException("kjt.refund.request.param.notify.url.invalid");
			}
		}
	}

	public String getInnerChannel() {
		return "kjtpay";
	}

	public RedirectInfo refund(Long refundTrackId, String channel, String tradeNo, String refundNo, String paymentCode,
			Integer refundAmount, String refundNotifyUrl, String reason, KjtToken kjtToken) {
		RedirectInfo info = new RedirectInfo();

		try {
			PayParamValid.validRefundParam(refundTrackId, channel, tradeNo, refundNo, paymentCode, refundAmount);
			this.checkParam(null, refundNotifyUrl, null, kjtToken, channel,
					Integer.valueOf(TradeType.REFUND.value()));
			KjtRefundRequest kjtPayRefundRequest = KjtRefundRequest.build(kjtToken).setOuter(refundNo)
					.setOrigOuter(tradeNo).setAmount(NumberUtils.formatPrice(refundAmount)).notify(refundNotifyUrl);
			String url = kjtPayRefundRequest.url();
			info.setResult(url);
			info.setIsRedirectNow(Boolean.TRUE);
			info.setChannel(channel);
		} catch (PayRequestParamException var13) {
			log.error("assembly kjtpay refund request param fail,error:{}", var13.getMessage());
			info.setError(var13.getMessage());
		} catch (Exception var14) {
			log.error("assembly kjtpay refund request param fail,cause:{}", Throwables.getStackTraceAsString(var14));
			info.setError("assembly.kjtpay.refund.request.param.fail");
		}

		return info;
	}

	public RedirectInfo syncThridTrans(Map<String, Object> context, KjtToken kjtToken) {
		RedirectInfo info = new RedirectInfo();
		try {
			Preconditions.checkArgument(!Arguments.isNull(context), "sysc.thrid.trans.context.is.null");
			Preconditions.checkArgument(context.containsKey("date"), "sysc.thrid.trans.context.date.is.null");
			Preconditions.checkArgument(context.containsKey("pageNo"), "sysc.thrid.trans.context.pageNo.is.null");
			this.checkParam(null, null, null, kjtToken, "kjtpay",
					Integer.valueOf(TradeType.SYNC_SETTLEMENT.value()));
			Date date = (Date) context.get("start");
			Integer pageNo = (Integer) context.get("pageNo");
			String url = KjtPageQueryRequest.build(kjtToken).settleDate(date).pageNo(pageNo.toString()).url();
			info.setResult(url);
			info.setChannel("kjtpay");
			info.setIsRedirectNow(Boolean.TRUE);
		} catch (IllegalArgumentException var8) {
			log.error("assembly kjtpay trans request param fail,error: {}", var8.getMessage());
			info.setError(var8.getMessage());
		} catch (Exception var9) {
			log.error("assembly kjtpay trans request param fail,cause: {}", Throwables.getStackTraceAsString(var9));
			info.setError("assembly.kjtpay.trans.reqeust.param.fail");
		}

		return info;
	}

	public KjtpayTransInfo initTrans(List<String> list) {
		KjtpayTransInfo kjtpayTrans = new KjtpayTransInfo();
		kjtpayTrans.setAmount(list.get(6));
		kjtpayTrans.setInnerNo(list.get(2));
		kjtpayTrans.setOuterNo(list.get(0));
		kjtpayTrans.setOrigOuterNo(list.get(1));
		kjtpayTrans.setType(list.get(3));
		kjtpayTrans.setOrderAt(DateTime.parse(list.get(4), NUM_FMT).toDate());
		kjtpayTrans.setPaidAt(DateTime.parse(list.get(5), NUM_FMT).toDate());
		kjtpayTrans.setRate(calcRate(list.get(6), list.get(7)));
		kjtpayTrans.setRateFee(list.get(7));
		kjtpayTrans.setStatus(list.get(8));
		kjtpayTrans.setCreatedAt(new Date());
		kjtpayTrans.setUpdatedAt(new Date());
		return kjtpayTrans;
	}

	private static String calcRate(String amount, String rateFee) {
		BigDecimal kjtAmount = new BigDecimal(amount);
		BigDecimal kjtRateFee = new BigDecimal(rateFee);
		BigDecimal result = kjtRateFee.divide(kjtAmount, 4, 0);
		return result.toString();
	}

	public boolean match(String channel) {
		return !Strings.isNullOrEmpty(channel)
				? (channel.contains("kjtpay") ? Boolean.TRUE.booleanValue() : Boolean.FALSE.booleanValue())
				: Boolean.FALSE.booleanValue();
	}
}
