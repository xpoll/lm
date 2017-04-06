package cn.blmdz.wolf.web.msg.config.test;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.wolf.web.msg.MsgWebService;
import cn.blmdz.wolf.web.msg.test.AppPushController;

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
