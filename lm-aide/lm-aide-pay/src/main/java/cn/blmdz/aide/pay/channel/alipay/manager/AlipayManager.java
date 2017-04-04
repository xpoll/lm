package cn.blmdz.aide.pay.channel.alipay.manager;

import java.util.Date;
import java.util.List;

import org.joda.time.DateTime;
import org.joda.time.Minutes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Objects;
import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;

import cn.blmdz.aide.pay.channel.alipay.dto.AlipayRefundData;
import cn.blmdz.aide.pay.channel.alipay.dto.RedirectInfo;
import cn.blmdz.aide.pay.channel.alipay.enums.RefundInterfaceType;
import cn.blmdz.aide.pay.channel.alipay.request.AlipayRequest;
import cn.blmdz.aide.pay.channel.alipay.request.AlipayToken;
import cn.blmdz.aide.pay.channel.alipay.request.CallBack;
import cn.blmdz.aide.pay.channel.alipay.request.MobileNewPayRequest;
import cn.blmdz.aide.pay.channel.alipay.request.PageQueryRequest;
import cn.blmdz.aide.pay.channel.alipay.request.RefundRequest;
import cn.blmdz.aide.pay.enums.TradeType;
import cn.blmdz.aide.pay.exception.PayRequestParamException;
import cn.blmdz.aide.pay.utils.PayParamValid;
import cn.blmdz.home.common.util.Arguments;

public class AlipayManager {
	private static final Logger log = LoggerFactory.getLogger(AlipayManager.class);

	public RedirectInfo pay(String channel, String subject, String content, String tradeNo, String systemNo,
			Date expiredAt, Integer fee, String notifyUrl, String returnUrl, AlipayToken alipayToken) {
		RedirectInfo info = new RedirectInfo();

		try {
			this.checkParam(notifyUrl, returnUrl, (String) null, (String) null, alipayToken, channel,
					Integer.valueOf(TradeType.PAY.value()));
			CallBack notify = new CallBack(notifyUrl);
			CallBack forward = new CallBack(returnUrl);
			if ("wapalipay".equals(channel)) {
				alipayToken.setWapGateway(alipayToken.getGateway());
				String url = MobileNewPayRequest.build(alipayToken).title(subject).content(content)
						.outerTradeNo(tradeNo).total(fee).notify(notify)
						.timeoutM(Minutes.minutesBetween(new DateTime(expiredAt), DateTime.now()).getMinutes() - 30)
						.forward(forward, systemNo).url();
				info.setChannel(channel);
				info.setResult(url);
				info.setIsRedirectNow(Boolean.TRUE);
				return info;
			}

			if ("alipay".equals(channel)) {
				AlipayRequest alipayRequest = AlipayRequest.build(alipayToken).title(subject).content(content)
						.outerTradeNo(tradeNo).total(fee).notify(notify)
						.timeoutM(Minutes.minutesBetween(new DateTime(expiredAt), DateTime.now()).getMinutes() - 30)
						.forward(forward, systemNo);
				info.setChannel(channel);
				info.setResult(alipayRequest.url());
				info.setIsRedirectNow(Boolean.TRUE);
			}
		} catch (PayRequestParamException var15) {
			log.error("assembly pay request fail,error:{}", var15.getMessage());
			info.setError(var15.getMessage());
		} catch (Exception var16) {
			log.error("assembly pay request fail,cause:{}", Throwables.getStackTraceAsString(var16));
			info.setError("assembly.pay.request.fail");
		}

		return info;
	}

	public RedirectInfo refund(Long refundTrackId, String reason, String channel, String tradeNo, String refundNo,
			String paymentCode, Integer refundAmount, String refundType, String refundNotifyUrl,
			AlipayToken alipayToken) {
		RedirectInfo info = new RedirectInfo();

		try {
			PayParamValid.validRefundParam(refundTrackId, channel, tradeNo, refundNo, paymentCode, refundAmount);
			this.checkParam((String) null, (String) null, refundNotifyUrl, refundType, alipayToken, channel,
					Integer.valueOf(TradeType.REFUND.value()));
			CallBack notify = new CallBack(refundNotifyUrl);
			List<AlipayRefundData> refundDetails = Lists.newArrayList();
			refundDetails.add(
					new AlipayRefundData(paymentCode, refundAmount, Strings.isNullOrEmpty(reason) ? "退款" : reason));
			if (Objects.equal(RefundInterfaceType.NEEDPASSWORD.value(), refundType)) {
				String url = RefundRequest.buildWithPwd(alipayToken).batch(refundNo).notify(notify)
						.detail(refundDetails).url();
				info.setIsRedirectNow(Boolean.TRUE);
				info.setResult(url);
				info.setChannel(channel);
			} else {
				String url = RefundRequest.buildWithNoPwd(alipayToken).batch(refundNo).notify(notify)
						.detail(refundDetails).url();
				info.setIsRedirectNow(Boolean.TRUE);
				info.setResult(url);
				info.setChannel(channel);
			}
		} catch (PayRequestParamException var15) {
			log.error("assembly refund request fail, error:{}", var15.getMessage());
			info.setError(var15.getMessage());
		} catch (Exception var16) {
			log.error("assembly refund request fail, error:{}", var16.getMessage());
			info.setError("assembly.refund.request.fail");
		}

		return info;
	}

	public RedirectInfo syncThridTrans(Date start, Date end, Integer pageNo, Integer pageSize,
			AlipayToken alipayToken) {
		RedirectInfo info = new RedirectInfo();

		try {
			Preconditions.checkArgument(Arguments.notNull(start), "sysc.thrid.trans.context.start.is.null");
			Preconditions.checkArgument(Arguments.notNull(end), "sysc.thrid.trans.context.end.is.null");
			Preconditions.checkArgument(Arguments.notNull(pageNo), "sysc.thrid.trans.context.pageNo.is.null");
			Preconditions.checkArgument(Arguments.notNull(pageSize), "sysc.thrid.trans.context.pageSize.is.null");
			this.checkParam((String) null, (String) null, (String) null, (String) null, alipayToken, "alipay",
					Integer.valueOf(TradeType.SYNC_SETTLEMENT.value()));
			String url = PageQueryRequest.build(alipayToken).start(start).end(end).pageNo(pageNo).pageSize(pageSize)
					.queryUrl();
			info.setResult(url);
			info.setIsRedirectNow(Boolean.TRUE);
			info.setChannel("alipay");
		} catch (IllegalArgumentException var8) {
			log.error("Sync alipay trans failed, cause:{}", var8.getMessage());
			info.setError(var8.getMessage());
		} catch (Exception var9) {
			log.error("Sync alipay trans failed, cause:{}", Throwables.getStackTraceAsString(var9));
			info.setError("sync.alipay.trans.fail");
		}

		return info;
	}

	private void checkParam(String notifyUrl, String returnUrl, String refundNotifyUrl, String refundType,
			AlipayToken alipayToken, String channel, Integer type) {
		if (Strings.isNullOrEmpty(alipayToken.getPid())) {
			throw new PayRequestParamException("pay.request.param.pid.invalid");
		} else if (Strings.isNullOrEmpty(alipayToken.getKey())) {
			throw new PayRequestParamException("pay.request.param.key.invalid");
		} else if (Strings.isNullOrEmpty(alipayToken.getAccount())) {
			throw new PayRequestParamException("pay.request.param.account.invalid");
		} else {
			if (type.equals(Integer.valueOf(TradeType.PAY.value()))) {
				if ("wapalipay".equals(channel)) {
					if (Strings.isNullOrEmpty(alipayToken.getWapGateway())) {
						throw new PayRequestParamException("pay.request.param.wap.gateway.invalid");
					}
				} else if (Strings.isNullOrEmpty(alipayToken.getGateway())) {
					throw new PayRequestParamException("pay.request.param.gateway.invalid");
				}

				if (Strings.isNullOrEmpty(notifyUrl)) {
					throw new PayRequestParamException("pay.request.param.notify.url.invalid");
				}

				if (Strings.isNullOrEmpty(returnUrl)) {
					throw new PayRequestParamException("pay.request.param.return.url.invalid");
				}
			}

			if (type.equals(Integer.valueOf(TradeType.REFUND.value()))) {
				if (Strings.isNullOrEmpty(refundNotifyUrl)) {
					throw new PayRequestParamException("refund.request.param.notify.url.invalid");
				}

				if (Strings.isNullOrEmpty(refundType)) {
					throw new PayRequestParamException("refund.request.param.type.invalid");
				}

				if (Strings.isNullOrEmpty(alipayToken.getGateway())) {
					throw new PayRequestParamException("pay.request.param.gateway.invalid");
				}
			}

		}
	}

	public String getInnerChannel() {
		return "alipay";
	}

	public boolean match(String channel) {
		return !Strings.isNullOrEmpty(channel)
				? (channel.contains("alipay") ? Boolean.TRUE.booleanValue() : Boolean.FALSE.booleanValue())
				: Boolean.FALSE.booleanValue();
	}
}
