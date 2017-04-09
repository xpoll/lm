package cn.blmdz.boot.dubbo.properties;

import lombok.Data;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "dubbo")
@Data
public class DubboProperties {
	private String name;
	private String registry = "zookeeper://127.0.0.1:2181";
	private Integer port = Integer.valueOf(-1);
	private Integer timeout = Integer.valueOf(1000);
	private Integer threads = Integer.valueOf(2);
	private Integer heartBeats = Integer.valueOf(30000);
	private String host;
	private String serialization;
	private String version;
	private String mode = "provider";
}
