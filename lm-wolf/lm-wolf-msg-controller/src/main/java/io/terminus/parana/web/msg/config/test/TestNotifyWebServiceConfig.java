package io.terminus.parana.web.msg.config.test;

import io.terminus.parana.web.msg.MsgWebService;
import io.terminus.parana.web.msg.test.NotifyController;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class TestNotifyWebServiceConfig {
   @Bean
   @ConditionalOnBean(
      name = {"notifyWebService"}
   )
   @ConditionalOnProperty({"msg.test.enabled"})
   public NotifyController notifyController(@Qualifier("notifyWebService") MsgWebService notifyWebService) {
      return new NotifyController(notifyWebService);
   }
}
