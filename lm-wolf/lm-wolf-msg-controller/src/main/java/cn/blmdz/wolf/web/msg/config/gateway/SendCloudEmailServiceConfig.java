	package cn.blmdz.wolf.web.msg.config.gateway;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.aide.email.EmailService;
import cn.blmdz.aide.email.impl.sendcloud.SendCloudEmailService;

@Configuration
public class SendCloudEmailServiceConfig {
   @Bean
   public EmailService sendCloudEmailService(@Value("${msg.sendcloud.user:user}") String user, @Value("${msg.sendcloud.triggerUser:triggerUser}") String triggerUser, @Value("${msg.sendcloud.batchUser:batchUser}") String batchUser, @Value("${msg.sendcloud.key:defaultkey}") String key, @Value("${msg.sendcloud.gateway:gateway}") String gateway) {
      return new SendCloudEmailService(user, triggerUser, batchUser, key, gateway);
   }
}
