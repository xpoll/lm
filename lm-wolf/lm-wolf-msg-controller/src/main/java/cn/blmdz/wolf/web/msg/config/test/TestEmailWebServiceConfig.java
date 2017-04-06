package cn.blmdz.wolf.web.msg.config.test;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.wolf.web.msg.MsgWebService;
import cn.blmdz.wolf.web.msg.test.EmailController;

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
