package cn.blmdz.wolf.msg.impl;

import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.wolf.msg.component.MessagePostHandler;
import cn.blmdz.wolf.msg.component.MessagePreHandler;
import cn.blmdz.wolf.msg.component.MessageValidatorChain;
import cn.blmdz.wolf.msg.component.SubscriptionChecker;
import cn.blmdz.wolf.msg.component.handler.DefaultMessagePostHandler;
import cn.blmdz.wolf.msg.component.handler.DefaultMessagePreHandler;
import cn.blmdz.wolf.msg.component.validate.DefaultMessageValidatorChain;
import cn.blmdz.wolf.msg.impl.component.helper.DefaultSubscriptionChecker;
import cn.blmdz.wolf.msg.impl.dao.mysql.SubscriptionDao;

@Configuration
@ComponentScan({"cn.blmdz.wolf.msg.impl"})
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
