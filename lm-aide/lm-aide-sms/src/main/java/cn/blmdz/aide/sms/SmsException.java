package cn.blmdz.aide.sms;

public class SmsException extends RuntimeException {
	private static final long serialVersionUID = 1L;

	public SmsException() {
	}

	public SmsException(String s) {
		super(s);
	}

	public SmsException(String s, Throwable throwable) {
		super(s, throwable);
	}

	public SmsException(Throwable throwable) {
		super(throwable);
	}
}
