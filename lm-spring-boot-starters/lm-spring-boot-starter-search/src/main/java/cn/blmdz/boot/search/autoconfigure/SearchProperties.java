package cn.blmdz.boot.search.autoconfigure;

import lombok.Data;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "search")
@Data
public class SearchProperties {
	private String host;
	private Integer port;
}
