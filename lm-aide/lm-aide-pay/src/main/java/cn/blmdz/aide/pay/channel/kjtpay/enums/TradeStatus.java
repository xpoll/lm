package cn.blmdz.aide.pay.channel.kjtpay.enums;

public enum TradeStatus {
	WAIT_BUYER_PAY("WAIT_BUYER_PAY", "等待买家付款"),
	PAY_FINISHED("PAY_FINISHED", "买家已付款"),
	TRADE_SUCCESS("TRADE_SUCCESS", "交易成功"),
	TRADE_FINISHED("TRADE_FINISHED", "交易结束"),
	REFUND_SUCCESS("REFUND_SUCCESS", "退款成功"),
	REFUND_FAIL("REFUND_FAIL", "退款失败"),
	TRANSFER_FAIL("TRANSFER_FAIL", "交易失败"),
	TRADE_CLOSED("TRADE_CLOSED", "交易关闭");

	private final String name;
	private final String description;

	private TradeStatus(String value, String description) {
		this.name = value;
		this.description = description;
	}

	public static TradeStatus from(String name) {
		for (TradeStatus status : values()) {
			if (status.name.equals(name)) {
				return status;
			}
		}

		throw new IllegalArgumentException("Illegal TradeStatus name:[" + name + "]");
	}

	public static TradeStatus fromDes(String description) {
		for (TradeStatus status : values()) {
			if (status.description.equals(description)) {
				return status;
			}
		}

		throw new IllegalArgumentException("Illegal TradeStatus description:[" + description + "]");
	}

	public String getName() {
		return this.name;
	}

	public String getDescription() {
		return this.description;
	}
}
