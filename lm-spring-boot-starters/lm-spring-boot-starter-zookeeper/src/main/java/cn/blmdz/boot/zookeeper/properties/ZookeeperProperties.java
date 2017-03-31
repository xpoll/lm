package cn.blmdz.boot.zookeeper.properties;

import lombok.Data;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "zookeeper")
@Data
public class ZookeeperProperties {
   private String host;
   private int port = 2181;
}
