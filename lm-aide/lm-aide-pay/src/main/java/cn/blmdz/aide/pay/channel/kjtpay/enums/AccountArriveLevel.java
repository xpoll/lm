package cn.blmdz.aide.pay.channel.kjtpay.enums;

public enum AccountArriveLevel {
	NORMAL("0", "普通"), FAST("1", "快速");

	private final String name;
	private final String description;

	private AccountArriveLevel(String value, String description) {
		this.name = value;
		this.description = description;
	}

	public static AccountArriveLevel from(String name) {
		for (AccountArriveLevel ch : values()) {
			if (ch.name.equals(name)) {
				return ch;
			}
		}

		throw new IllegalArgumentException("Illegal AccountArriveLevel name:[" + name + "]");
	}

	public static AccountArriveLevel fromDes(String description) {
		for (AccountArriveLevel ch : values()) {
			if (ch.description.equals(description)) {
				return ch;
			}
		}

		throw new IllegalArgumentException("Illegal AccountArriveLevel description:[" + description + "]");
	}
}
