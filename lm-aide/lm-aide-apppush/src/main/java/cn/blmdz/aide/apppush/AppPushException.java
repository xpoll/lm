package cn.blmdz.aide.apppush;

public class AppPushException extends RuntimeException {
	private static final long serialVersionUID = 1L;

	public AppPushException() {
	}

	public AppPushException(String message, Throwable cause) {
		super(message, cause);
	}

	public AppPushException(String message) {
		super(message);
	}

	public AppPushException(Throwable cause) {
		super(cause);
	}

	public AppPushException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}
