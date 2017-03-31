package cn.blmdz.boot.search.autoconfigure;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.home.search.ESClient;
import cn.blmdz.home.search.api.Searcher;

@Configuration
@ConditionalOnClass({ Searcher.class })
@EnableConfigurationProperties({ SearchProperties.class })
public class ESSearchAutoConfiguration {
	private static final Logger log = LoggerFactory.getLogger(ESSearchAutoConfiguration.class);
	@Autowired
	private SearchProperties properties;

	@Bean
	public ESClient esClient() {
		log.info("Auto Configure Search configuration");
		log.info("host       --> {}", this.properties.getHost());
		log.info("port       --> {}", this.properties.getPort());
		return new ESClient(this.properties.getHost(), this.properties
				.getPort().intValue());
	}
}
