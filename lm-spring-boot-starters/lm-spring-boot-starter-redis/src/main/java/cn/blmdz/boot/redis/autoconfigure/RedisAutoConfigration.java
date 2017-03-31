package cn.blmdz.boot.redis.autoconfigure;

import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.google.common.base.Splitter;
import com.google.common.collect.Sets;

import cn.blmdz.boot.redis.properties.RedisProperties;
import cn.blmdz.home.common.redis.utils.JedisTemplate;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;
import redis.clients.jedis.JedisPoolConfig;
import redis.clients.jedis.JedisSentinelPool;
import redis.clients.util.Pool;

@Configuration
@EnableConfigurationProperties({ RedisProperties.class })
public class RedisAutoConfigration {
	private static final Logger log = LoggerFactory
			.getLogger(RedisAutoConfigration.class);
	@Autowired
	private RedisProperties properties;

	@Bean
	public JedisPoolConfig jedisPoolConfig() {
		JedisPoolConfig config = new JedisPoolConfig();
		config.setMaxTotal(this.properties.getMaxTotal());
		config.setMaxIdle(this.properties.getMaxIdle());
		config.setMaxWaitMillis(properties.getMaxWaitMillis());
		config.setTestOnBorrow(this.properties.isTestOnBorrow());
		return config;
	}

	@Bean
	public Pool<Jedis> jedisPool() {
		if (this.properties.isCluster()) {
			String sentinelProps = this.properties.getSentinelHosts();
			Iterable<String> parts = Splitter.on(',').trimResults()
					.omitEmptyStrings().split(sentinelProps);
			Set<String> sentinelHosts = Sets.newHashSet(parts);
			String masterName = this.properties.getSentinelMasterName();
			return new JedisSentinelPool(masterName, sentinelHosts,
					this.jedisPoolConfig());
		} else {
			return new JedisPool(this.jedisPoolConfig(),
					this.properties.getHost(), this.properties.getPort());
		}
	}

	@Bean
	public JedisTemplate jedisTemplate() {
		return new JedisTemplate(this.jedisPool());
	}
}
