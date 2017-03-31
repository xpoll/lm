package cn.blmdz.home.zookeeper.autoconfigure;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.home.zookeeper.ZKClientFactory;
import cn.blmdz.home.zookeeper.leader.HostLeader;
import cn.blmdz.home.zookeeper.leader.LeadLatchInitiator;

@Configuration
public class ZKAutoConfiguration {
   private static final Logger log = LoggerFactory.getLogger(ZKAutoConfiguration.class);

   @ConditionalOnProperty({"curator.leader.path"})
   @Bean
   public ZKClientFactory zkClientFactory(@Value("${curator.zk.host}") String host, @Value("${curator.zk.port}") Integer port) throws Exception {
      log.info("curator.zk.host --> {}", host);
      log.info("curator.zk.port --> {}", port);
      String address = host + ":" + port;
      return new ZKClientFactory(address);
   }

   @ConditionalOnBean({ZKClientFactory.class})
   @Bean
   public LeadLatchInitiator leadLatchInitiator(ZKClientFactory zkClientFactory, @Value("${curator.leader.path}") String zkPath) throws Exception {
      log.info("curator.leader.path --> {}", zkPath);
      return new LeadLatchInitiator(zkClientFactory, zkPath);
   }

   @ConditionalOnBean({LeadLatchInitiator.class})
   @Bean
   public HostLeader hostLeader(LeadLatchInitiator leadLatchInitiator) throws Exception {
      HostLeader hostLeader = new HostLeader(leadLatchInitiator);
      hostLeader.init();
      return hostLeader;
   }

   @ConditionalOnMissingBean({LeadLatchInitiator.class})
   @Configuration
   protected static class DefaultHostLeaderConfiguration {
      @Bean
      public HostLeader hostLeader() {
         ZKAutoConfiguration.log.warn("use LOCALHOST as leader, it only should be used on DEV or TEST environment!");
         return new HostLeader();
      }
   }
}
