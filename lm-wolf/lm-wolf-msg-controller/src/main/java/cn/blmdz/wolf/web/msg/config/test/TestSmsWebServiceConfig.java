package cn.blmdz.wolf.web.msg.config.test;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.wolf.web.msg.MsgWebService;
import cn.blmdz.wolf.web.msg.test.SmsController;

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
