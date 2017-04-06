package cn.blmdz.boot.dubbo.autoconfigure;

import cn.blmdz.boot.dubbo.command.DubboServiceLatchCommandLineRunner;
import cn.blmdz.boot.dubbo.command.DubboServiceListenerBean;
import cn.blmdz.boot.dubbo.properties.DubboProperties;

import com.alibaba.dubbo.config.ApplicationConfig;
import com.alibaba.dubbo.config.ConsumerConfig;
import com.alibaba.dubbo.config.ProtocolConfig;
import com.alibaba.dubbo.config.RegistryConfig;
import com.google.common.base.MoreObjects;
import com.google.common.base.Strings;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.Order;
import org.springframework.util.SocketUtils;

@Order
@Configuration
@EnableConfigurationProperties({DubboProperties.class})
public class DubboAutoConfiguration {
   private static final Logger log = LoggerFactory.getLogger(DubboAutoConfiguration.class);
   @Autowired
   private DubboProperties dubboProperties;
   @Value("${spring.application.name:}")
   private String appName;

   @Bean(
      name = {"com.alibaba.dubbo.config.ApplicationConfig"}
   )
   public ApplicationConfig applicationConfig() {
      ApplicationConfig config = new ApplicationConfig();
      if(Strings.isNullOrEmpty(this.dubboProperties.getName()) && Strings.isNullOrEmpty(this.appName)) {
         throw new IllegalStateException("AppName can\'t be empty, please make sure that \'dubbo.name\' or \'spring.application.name\' have been set");
      } else {
         config.setName((String)MoreObjects.firstNonNull(this.dubboProperties.getName(), this.appName));
         config.setLogger("slf4j");
         return config;
      }
   }

   @Bean(
      name = {"com.alibaba.dubbo.config.RegistryConfig"}
   )
   public RegistryConfig registryConfig() {
      RegistryConfig config = new RegistryConfig();
      config.setAddress(this.dubboProperties.getRegistry());
      config.setProtocol("zookeeper");
      config.setTimeout(this.dubboProperties.getTimeout());
      config.setVersion(this.dubboProperties.getVersion());
      return config;
   }

   @Bean(
      name = {"com.alibaba.dubbo.config.ProtocolConfig"}
   )
   @ConditionalOnProperty(
      prefix = "dubbo",
      name = {"mode"},
      havingValue = "provider",
      matchIfMissing = true
   )
   public ProtocolConfig protocolConfig() {
      ProtocolConfig config = new ProtocolConfig();
      config.setName("dubbo");
      if(this.dubboProperties.getHost() != null) {
         config.setHost(this.dubboProperties.getHost());
      }

      if(this.dubboProperties.getSerialization() != null) {
         config.setSerialization(this.dubboProperties.getSerialization());
      }

      if(this.dubboProperties.getPort().intValue() == -1) {
         int port = SocketUtils.findAvailableTcpPort(30000);
         config.setPort(Integer.valueOf(port));
      } else {
         config.setPort(this.dubboProperties.getPort());
      }

      config.setThreads(this.dubboProperties.getThreads());
      config.setHeartbeat(this.dubboProperties.getHeartBeats());
      return config;
   }

   @Bean
   public ConsumerConfig consumerConfig() {
      ConsumerConfig config = new ConsumerConfig();
      config.setTimeout(Integer.valueOf(10000));
      return config;
   }

   @Bean
   @ConditionalOnProperty(
      prefix = "dubbo",
      name = {"mode"},
      havingValue = "provider",
      matchIfMissing = true
   )
   public DubboServiceLatchCommandLineRunner configureDubboServiceLatchCommandLineRunner() {
      log.info("Auto start dubbo configuration");
      log.info("port       --> {}", this.dubboProperties.getPort());
      log.info("registry   --> {}", this.dubboProperties.getRegistry());
      log.info("threads    --> {}", this.dubboProperties.getThreads());
      log.info("timeout    --> {}", this.dubboProperties.getTimeout());
      log.info("heartBeats --> {}", this.dubboProperties.getHeartBeats());
      log.info("host       --> {}", this.dubboProperties.getHost());
      log.info("serialization --> {}", this.dubboProperties.getSerialization());
      return new DubboServiceLatchCommandLineRunner();
   }

   @Bean
   public DubboServiceListenerBean dubboServiceListenerBean() {
      return new DubboServiceListenerBean((String)MoreObjects.firstNonNull(this.dubboProperties.getName(), this.appName));
   }
}
