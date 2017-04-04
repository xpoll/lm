package cn.blmdz.aide.pay.channel.alipay.request;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;

import lombok.AllArgsConstructor;

public class TradeCloseRequest extends Request {
	private static final Logger log = LoggerFactory.getLogger(TradeCloseRequest.class);

	private TradeCloseRequest(AlipayToken alipayToken) {
		super(alipayToken);
		this.params.put("service", "close_trade");
	}

	public static TradeCloseRequest build(AlipayToken alipayToken) {
		return new TradeCloseRequest(alipayToken);
	}

	public TradeCloseRequest tradeNo(String tradeNo) {
		this.params.put("trade_no", tradeNo);
		return this;
	}

	public TradeCloseRequest outOrderNo(String orderNo) {
		this.params.put("out_order_no", orderNo);
		return this;
	}

	public TradeCloseRequest role(TradeCloseRequest.Role role) {
		this.params.put("trade_role", role.code);
		return this;
	}

	public Boolean notifyToClose() {
		String url = super.url();
		log.debug("close trade url: {}", url);
		String body = HttpRequest.get(url).connectTimeout(10000).readTimeout(10000).body();
		log.debug("close trade result: {}", body);
		return this.convertToResponse(body);
	}

	@AllArgsConstructor
	public static enum Role {
		BUYER(1, "买家", "B"), SELLER(2, "卖家", "S");

		private final int value;
		private final String description;
		private final String code;

		public String code() {
			return this.code;
		}

		public int value() {
			return this.value;
		}

		public String toString() {
			return this.description;
		}
	}
}
