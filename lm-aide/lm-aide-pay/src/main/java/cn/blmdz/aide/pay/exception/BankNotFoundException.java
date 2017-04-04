package cn.blmdz.aide.pay.exception;

public class BankNotFoundException extends RuntimeException {
	private static final long serialVersionUID = 1L;

	public BankNotFoundException(String s) {
		super(s);
	}
}
