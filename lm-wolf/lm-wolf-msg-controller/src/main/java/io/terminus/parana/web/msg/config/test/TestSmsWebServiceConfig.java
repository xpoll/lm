package io.terminus.parana.web.msg.config.test;

import io.terminus.parana.web.msg.MsgWebService;
import io.terminus.parana.web.msg.test.SmsController;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class TestSmsWebServiceConfig {
   @Bean
   @ConditionalOnBean(
      name = {"smsWebService"}
   )
   @ConditionalOnProperty({"msg.test.enabled"})
   public SmsController smsController(@Qualifier("smsWebService") MsgWebService smsWebService) {
      return new SmsController(smsWebService);
   }
}
