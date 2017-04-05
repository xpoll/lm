package io.terminus.parana.config.impl;

import io.terminus.zookeeper.ZKClientFactory;
import io.terminus.zookeeper.pubsub.Publisher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

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
