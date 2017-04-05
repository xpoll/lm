package io.terminus.parana.web.msg.config.gateway;

import io.terminus.lib.apppush.AppPushService;
import io.terminus.lib.apppush.impl.umeng.UmengAppPushService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class UmengAppPushServiceConfig {
   @Bean
   public AppPushService umengAppPushServiceAndroid(@Value("${msg.umeng.android.appKey:defaultKey}") String appKey, @Value("${msg.umeng.android.appSecret:defaultSecret}") String appSecret) {
      return new UmengAppPushService(appKey, appSecret);
   }

   @Bean
   public AppPushService umengAppPushServiceIos(@Value("${msg.umeng.ios.appKey:defaultKey}") String appKey, @Value("${msg.umeng.ios.appSecret:defaultSecret}") String appSecret) {
      return new UmengAppPushService(appKey, appSecret);
   }
}
