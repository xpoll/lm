package io.terminus.parana.web.msg.config.simple;

import io.terminus.parana.msg.component.MessagePostHandler;
import io.terminus.parana.msg.component.MessagePreHandler;
import io.terminus.parana.msg.component.MessageValidatorChain;
import io.terminus.parana.msg.component.handler.DefaultMessagePostHandler;
import io.terminus.parana.msg.component.handler.DefaultMessagePreHandler;
import io.terminus.parana.msg.component.validate.DefaultMessageValidatorChain;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

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
