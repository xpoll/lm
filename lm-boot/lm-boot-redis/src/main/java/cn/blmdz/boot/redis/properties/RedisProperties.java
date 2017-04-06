package cn.blmdz.boot.redis.properties;

import lombok.Data;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "redis")
@Data
public class RedisProperties {
	private int port = 6379;
	private int maxTotal = 5;
	private int maxIdle = 0;
	private int maxWaitMillis = 10000;
	private boolean testOnBorrow = true;
	private boolean cluster = false;
	private String host;
	private String sentinelHosts;
	private String sentinelMasterName;
}
