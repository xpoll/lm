package cn.blmdz.boot.wolf.autoconfigure;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.google.common.eventbus.EventBus;

import cn.blmdz.home.zookeeper.ZKClientFactory;
import cn.blmdz.wolf.config.ConfigCenter;
import cn.blmdz.wolf.config.ConfigUpdateListener;

@Configuration
@EnableConfigurationProperties({ConfigProperties.class})
public class ConfigAutoConfiguation {
   private static final Logger log = LoggerFactory.getLogger(ConfigAutoConfiguation.class);

   @Bean
   public ConfigCenter configCenter() {
      return new ConfigCenter();
   }

   @Bean
   @ConditionalOnProperty(
      prefix = "parana.config",
      name = {"mode"},
      havingValue = "server",
      matchIfMissing = false
   )
   public ConfigUpdateListener configUpdateListener() {
      return new ConfigUpdateListener();
   }

   @Bean
   @ConditionalOnClass({ConfigUpdateListener.class})
   public EventBus eventBus() {
      return new EventBus();
   }

   @Bean
   @ConditionalOnClass({ConfigUpdateListener.class})
   public ZKClientFactory zkClientFactory(@Value("${zookeeper.host}") String host, @Value("${zookeeper.port}") int port) throws Exception {
      return new ZKClientFactory(host + ":" + port);
   }
}
