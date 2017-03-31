package cn.blmdz.hunt.engine.utils;

public enum Protocol {
	   HTTP("http://"),
	   HTTPS("https://"),
	   SERVLET("resource:"),
	   CLASSPATH("classpath:"),
	   NONE(""),
	   FILE("file:"),
	   ZK("zookeeper://"),
	   REDIS("redis:");

	private String prefix;

	private Protocol(String prefix) {
		this.prefix = prefix;
	}

	public static Protocol analyze(String uri) {
		String lowerUri = uri.toLowerCase();
		return lowerUri.startsWith(FILE.prefix) ? FILE
				: (lowerUri.startsWith(HTTP.prefix) ? HTTP
						: (lowerUri.startsWith(HTTPS.prefix) ? HTTPS : (lowerUri.startsWith(SERVLET.prefix) ? SERVLET
								: (lowerUri.startsWith(CLASSPATH.prefix) ? CLASSPATH
										: (lowerUri.startsWith(ZK.prefix) ? ZK
												: (lowerUri.startsWith(REDIS.prefix) ? REDIS : NONE))))));
	}

	public static String removeProtocol(String uri) {
		return removeProtocol(uri, analyze(uri));
	}

	public static String removeProtocol(String uri, Protocol protocol) {
		return uri.substring(protocol.prefix.length());
	}

	public String getPrefix() {
		return this.prefix;
	}
}
