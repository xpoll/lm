package io.terminus.parana.web.msg.config.test;

import io.terminus.parana.web.msg.MsgWebService;
import io.terminus.parana.web.msg.test.AppPushController;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class TestAppPushWebServiceConfig {
   @Bean
   @ConditionalOnBean(
      name = {"appPushWebService"}
   )
   @ConditionalOnProperty({"msg.test.enabled"})
   public AppPushController appPushController(@Qualifier("appPushWebService") MsgWebService appPushWebService) {
      return new AppPushController(appPushWebService);
   }
}
