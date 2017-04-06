	package cn.blmdz.wolf.web.msg.config.gateway;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.aide.sms.SmsService;
import cn.blmdz.aide.sms.impl.alibaba.AliSmsService;

@Configuration
public class AliSmsServiceConfig {
   @Bean
   public SmsService aliSmsService(@Value("${msg.alisms.appKey:defaultKey}") String appKey, @Value("${msg.alisms.appSecret:defaultSecret}") String appSecret, @Value("${msg.alisms.smsUrl:http://gw.api.taobao.com/router/rest}") String smsUrl, @Value("${msg.alisms.version:2.0}") String version) {
      return new AliSmsService(appKey, appSecret, smsUrl, version);
   }
}
