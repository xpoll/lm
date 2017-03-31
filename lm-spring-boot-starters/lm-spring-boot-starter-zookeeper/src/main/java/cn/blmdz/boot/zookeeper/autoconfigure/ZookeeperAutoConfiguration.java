package cn.blmdz.boot.zookeeper.autoconfigure;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.boot.zookeeper.properties.ZookeeperProperties;
import cn.blmdz.home.zookeeper.ZKClientFactory;

@Configuration
@EnableConfigurationProperties(ZookeeperProperties.class)
public class ZookeeperAutoConfiguration {
	private static final Logger log = LoggerFactory
			.getLogger(ZookeeperAutoConfiguration.class);
	@Autowired
	private ZookeeperProperties properties;

	@Bean
	public ZKClientFactory zkClientFactory() throws Exception {
		String address = properties.getHost() + ":" + properties.getPort();
		return new ZKClientFactory(address);
	}
}
