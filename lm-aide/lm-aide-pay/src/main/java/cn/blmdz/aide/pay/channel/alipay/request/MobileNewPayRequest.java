package cn.blmdz.aide.pay.channel.alipay.request;

import java.net.URLEncoder;

import com.google.common.base.Preconditions;
import com.google.common.base.Strings;

import cn.blmdz.home.common.util.Arguments;

public class MobileNewPayRequest extends Request {
	protected MobileNewPayRequest(AlipayToken alipayToken) {
		super(alipayToken);
		this.params.put("service", "alipay.wap.create.direct.pay.by.user");
		this.params.put("payment_type", "1");
		this.params.put("seller_id", alipayToken.getPid());
		this.params.remove("seller_email");
	}

	public static MobileNewPayRequest build(AlipayToken alipayToken) {
		return new MobileNewPayRequest(alipayToken);
	}

	public MobileNewPayRequest notify(CallBack notify) {
		if (Arguments.notNull(notify)) {
			this.params.put("notify_url", notify);
		}

		return this;
	}

	public MobileNewPayRequest forward(CallBack forward, String systemNo) {
		if (Arguments.notNull(forward)) {
			this.params.put("return_url", forward + "?data=" + systemNo);
		}

		return this;
	}

	public MobileNewPayRequest outerTradeNo(String outerTradeNo) {
		Preconditions.checkArgument(Arguments.notEmpty(outerTradeNo), "alipay.pay.outer.trade.no.empty");
		this.params.put("out_trade_no", outerTradeNo);
		return this;
	}

	public MobileNewPayRequest title(String title) {
		if (title != null) {
			this.params.put("subject", title);
		}

		return this;
	}

	public MobileNewPayRequest total(Integer total) {
		Preconditions.checkArgument(Arguments.notNull(total), "alipay.pay.total.empty");
		String fee = DECIMAL_FORMAT.format((double) total.intValue() / 100.0D);
		this.params.put("total_fee", fee);
		return this;
	}

	public MobileNewPayRequest paymentType(String type) {
		if (Strings.isNullOrEmpty(type)) {
			this.params.put("payment_type", "1");
		} else {
			this.params.put("payment_type", type);
		}

		return this;
	}

	public MobileNewPayRequest content(String content) {
		if (Arguments.notEmpty(content)) {
			this.params.put("body", content);
		}

		return this;
	}

	public MobileNewPayRequest show(CallBack show) {
		if (Arguments.notNull(show)) {
			this.params.put("show_url", show);
		}

		return this;
	}

	public MobileNewPayRequest timeoutM(int minutes) {
		if (minutes > 0 && minutes <= 21600) {
			this.params.put("it_b_pay", minutes + "m");
		}

		return this;
	}

	public void sign() {
		try {
			super.sign();
			String subject = (String) this.params.get("subject");
			if (!Strings.isNullOrEmpty(subject)) {
				this.params.put("subject", URLEncoder.encode(subject, "utf-8"));
			}

			String body = (String) this.params.get("body");
			if (!Strings.isNullOrEmpty(body)) {
				this.params.put("body", URLEncoder.encode(body, "utf-8"));
			}

		} catch (Exception var3) {
			throw new RuntimeException(var3);
		}
	}

	public String pay() {
		return super.url();
	}
}
