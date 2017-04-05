package io.terminus.parana.web.msg.config.test;

import io.terminus.parana.web.msg.MsgWebService;
import io.terminus.parana.web.msg.test.EmailController;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class TestEmailWebServiceConfig {
   @Bean
   @ConditionalOnBean(
      name = {"emailWebService"}
   )
   @ConditionalOnProperty({"msg.test.enabled"})
   public EmailController emailController(@Qualifier("emailWebService") MsgWebService emailWebService) {
      return new EmailController(emailWebService);
   }
}
