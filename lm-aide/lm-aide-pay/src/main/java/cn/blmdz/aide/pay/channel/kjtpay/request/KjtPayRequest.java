package cn.blmdz.aide.pay.channel.kjtpay.request;

import com.google.common.base.Preconditions;

import cn.blmdz.home.common.util.Arguments;

public class KjtPayRequest extends Request {
	private KjtPayRequest(KjtToken kjtToken) {
		super(kjtToken);
		this.params.put("memo", "");
		this.params.put("buyer_id", "anonymous");
		this.params.put("buyer_id_type", "1");
		this.params.put("go_cashier", "Y");
	}

	public static KjtPayRequest build(KjtToken kjtToken) {
		return new KjtPayRequest(kjtToken);
	}

	public KjtPayRequest service(String service) {
		if (Arguments.notNull(service)) {
			this.params.put("service", service);
		}

		return this;
	}

	public KjtPayRequest returnUrl(String forward) {
		if (Arguments.notNull(forward)) {
			this.params.put("return_url", forward);
		}

		return this;
	}

	public KjtPayRequest notify(String notify) {
		if (Arguments.notNull(notify)) {
			this.params.put("notify_url", notify);
		}

		return this;
	}

	public KjtPayRequest tradeList(String content) {
		if (Arguments.notEmpty(content)) {
			this.params.put("trade_list", content);
		}

		return this;
	}

	public KjtPayRequest payMethod(Integer fee, String bank, String type) {
		if (Arguments.notEmpty(type)) {
			String payMethodValue = "online_bank^" + formatFee(fee) + "^" + bank + "," + type + ",DC";
			this.params.put("pay_method", payMethodValue);
		} else {
			this.params.put("pay_method", "");
		}

		return this;
	}

	public KjtPayRequest requestNo(String requestNo) {
		Preconditions.checkArgument(Arguments.notEmpty(requestNo), "kjt.pay.outer.trade.no.empty");
		this.params.put("request_no", requestNo);
		return this;
	}

	public KjtPayRequest total(Integer total) {
		Preconditions.checkArgument(Arguments.notNull(total), "kjt.pay.total.empty");
		String fee = DECIMAL_FORMAT.format((double) total.intValue() / 100.0D);
		this.params.put("transfer_amount", fee);
		return this;
	}

	public KjtPayRequest setSign(String sign) {
		Preconditions.checkArgument(Arguments.notNull(sign), "kjt.pay.sign.empty");
		this.params.put("sign", sign);
		return this;
	}

	public KjtPayRequest setSignType(String type) {
		Preconditions.checkArgument(Arguments.notNull(type), "kjt.pay.sign_type.empty");
		this.params.put("sign_type", type);
		return this;
	}

	public String pay() {
		return super.url();
	}

	private static String formatFee(Integer fee) {
		return DECIMAL_FORMAT.format((double) fee.intValue() / 100.0D);
	}
}
