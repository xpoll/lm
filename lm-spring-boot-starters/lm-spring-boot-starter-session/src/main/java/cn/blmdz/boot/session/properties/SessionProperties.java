package cn.blmdz.boot.session.properties;

import lombok.Data;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "session")
@Data
public class SessionProperties {
	private String source = "redis";
	private String serializeType = "json";
	private String redisPrefix = "afsession";
	private boolean redisCluster = Boolean.FALSE.booleanValue();
	private boolean redisTestOnBorrow = Boolean.TRUE.booleanValue();
	private int redisMaxIdle = 0;
	private int redisMaxTotal = 5;
	private int redisPort = 6379;
	private int redisIndex = 0;
	private String cookieName = "msid";
	private int maxInactiveInterval = 1800;
	private int cookieMaxAge = -1;
	private String cookieContextPath = "/";
	private String redisHost;
	private String redisSentinelMasterName;
	private String redisSentinelHosts;
	private String cookieDomain;
}
