package cn.blmdz.boot.session.autoconfigure;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.embedded.FilterRegistrationBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.boot.session.properties.SessionProperties;
import cn.blmdz.home.session.AFSessionFilter;
import cn.blmdz.home.session.AFSessionManager;

@Configuration
@EnableConfigurationProperties({ SessionProperties.class })
public class SessionAutoConfiguration {
	private static final Logger log = LoggerFactory.getLogger(SessionAutoConfiguration.class);
	@Autowired
	private SessionProperties properties;

	@Bean
	public FilterRegistrationBean afSessionFilterRegistration() {
		log.debug("filter regist -> afSessionFilter");
		FilterRegistrationBean registration = new FilterRegistrationBean();
		AFSessionFilter afSessionFilter = new AFSessionFilter(true);
		afSessionFilter.setSessionManager(afSessionManager());
		afSessionFilter.setCookieDomain(properties.getCookieDomain());
		afSessionFilter.setMaxInactiveInterval(properties
				.getMaxInactiveInterval());
		afSessionFilter.setSessionCookieName(properties.getCookieName());
		afSessionFilter.setCookieContextPath(properties
				.getCookieContextPath());
		afSessionFilter.setCookieMaxAge(properties.getCookieMaxAge());
		afSessionFilter.setSessionKeyPrefix(properties.getRedisPrefix());
		registration.setFilter(afSessionFilter);
		registration.addUrlPatterns(new String[] { "/*" });
		registration.addInitParameter("sessionCookieName", "msid");
		registration.setName("AFSessionFilter");
		return registration;
	}

	@Bean
	public AFSessionManager afSessionManager() {
		cn.blmdz.home.session.util.Configuration configuration = new cn.blmdz.home.session.util.Configuration();
		configuration.setSerializeType(properties.getSerializeType());
		configuration.setSource(properties.getSource());
		configuration.setSessionRedisPrefix(properties.getRedisPrefix());
		configuration.setSessionRedisCluster(Boolean.valueOf(properties
				.isRedisCluster()));
		configuration.setSessionRedisTestOnBorrow(Boolean
				.valueOf(properties.isRedisTestOnBorrow()));
		configuration.setSessionRedisMaxIdle(Integer.valueOf(properties
				.getRedisMaxIdle()));
		configuration.setSessionRedisMaxTotal(Integer.valueOf(properties
				.getRedisMaxTotal()));
		configuration.setSessionRedisHost(properties.getRedisHost());
		configuration.setSessionRedisPort(Integer.valueOf(properties
				.getRedisPort()));
		configuration.setSessionRedisSentinelHosts(properties
				.getRedisSentinelHosts());
		configuration.setSessionRedisSentinelMasterName(properties
				.getRedisSentinelMasterName());
		configuration.setSessionRedisDbIndex(Integer.valueOf(properties
				.getRedisIndex()));
		return AFSessionManager.newInstance(configuration);
	}
}
