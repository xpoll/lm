package cn.blmdz.wolf.config.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.home.zookeeper.ZKClientFactory;
import cn.blmdz.home.zookeeper.pubsub.Publisher;

@Configuration
@ComponentScan({"io.terminus.parana.config.impl"})
public class ConfigAutoConfig {
   @Autowired
   private ZKClientFactory zkClientFactory;

   @Bean
   public Publisher publisher() throws Exception {
      return new Publisher(this.zkClientFactory, "/parana", "config");
   }
}
