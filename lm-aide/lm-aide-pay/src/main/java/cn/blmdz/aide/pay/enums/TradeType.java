package cn.blmdz.aide.pay.enums;

public enum TradeType {
	PAY(1, "支付"), REFUND(2, "退款"), SYNC_SETTLEMENT(3, "拉取账务"), TRANSFER(4, "转账");

	private final int value;
	private final String description;

	private TradeType(int value, String description) {
		this.value = value;
		this.description = description;
	}

	public int value() {
		return this.value;
	}

	public String toString() {
		return this.description;
	}
}
