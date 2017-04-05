package io.terminus.parana.msg.impl;

import io.terminus.parana.msg.component.MessagePostHandler;
import io.terminus.parana.msg.component.MessagePreHandler;
import io.terminus.parana.msg.component.MessageValidatorChain;
import io.terminus.parana.msg.component.SubscriptionChecker;
import io.terminus.parana.msg.component.handler.DefaultMessagePostHandler;
import io.terminus.parana.msg.component.handler.DefaultMessagePreHandler;
import io.terminus.parana.msg.component.validate.DefaultMessageValidatorChain;
import io.terminus.parana.msg.impl.component.helper.DefaultSubscriptionChecker;
import io.terminus.parana.msg.impl.dao.mysql.SubscriptionDao;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"io.terminus.parana.msg.impl"})
public class MessageAutoConfig {
   @Bean
   @ConditionalOnMissingBean({MessageValidatorChain.class})
   public MessageValidatorChain messageValidatorChain() {
      return new DefaultMessageValidatorChain();
   }

   @Bean
   @ConditionalOnMissingBean({MessagePreHandler.class})
   public MessagePreHandler messagePreHandler() {
      return new DefaultMessagePreHandler();
   }

   @Bean
   @ConditionalOnMissingBean({MessagePostHandler.class})
   public MessagePostHandler messagePostHandler() {
      return new DefaultMessagePostHandler();
   }

   @Bean
   @ConditionalOnProperty({"msg.check.subscribe"})
   public SubscriptionChecker subscriptionChecker(SubscriptionDao subscriptionDao) {
      return new DefaultSubscriptionChecker(subscriptionDao);
   }
}
