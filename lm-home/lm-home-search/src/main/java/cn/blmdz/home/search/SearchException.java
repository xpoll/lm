package cn.blmdz.home.search;

public class SearchException extends RuntimeException {
	private static final long serialVersionUID = 838438038108496483L;

	public SearchException() {
	}

	public SearchException(String message) {
		super(message);
	}

	public SearchException(String message, Throwable cause) {
		super(message, cause);
	}

	public SearchException(Throwable cause) {
		super(cause);
	}

	public SearchException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}
