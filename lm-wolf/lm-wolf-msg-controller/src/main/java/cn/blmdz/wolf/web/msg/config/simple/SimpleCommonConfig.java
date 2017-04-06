package cn.blmdz.wolf.web.msg.config.simple;

import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.wolf.msg.component.MessagePostHandler;
import cn.blmdz.wolf.msg.component.MessagePreHandler;
import cn.blmdz.wolf.msg.component.MessageValidatorChain;
import cn.blmdz.wolf.msg.component.handler.DefaultMessagePostHandler;
import cn.blmdz.wolf.msg.component.handler.DefaultMessagePreHandler;
import cn.blmdz.wolf.msg.component.validate.DefaultMessageValidatorChain;

@Configuration
public class SimpleCommonConfig {
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
}
