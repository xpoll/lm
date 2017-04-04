package cn.blmdz.aide.pay.channel.kjtpay.enums;

public enum AccountType {
	TOB("B", "对公"), TOC("C", "对私");

	private final String name;
	private final String description;

	private AccountType(String value, String description) {
		this.name = value;
		this.description = description;
	}

	public static AccountType from(String name) {
		for (AccountType ch : values()) {
			if (ch.name.equals(name)) {
				return ch;
			}
		}

		throw new IllegalArgumentException("Illegal AccountType name:[" + name + "]");
	}

	public static AccountType fromDes(String description) {
		for (AccountType ch : values()) {
			if (ch.description.equals(description)) {
				return ch;
			}
		}

		throw new IllegalArgumentException("Illegal AccountType description:[" + description + "]");
	}

	public String getName() {
		return this.name;
	}

	public String getDescription() {
		return this.description;
	}
}
