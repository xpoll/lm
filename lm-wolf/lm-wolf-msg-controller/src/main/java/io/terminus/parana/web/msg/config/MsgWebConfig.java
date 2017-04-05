package io.terminus.parana.web.msg.config;

import io.terminus.lib.apppush.AppPushService;
import io.terminus.lib.apppush.impl.umeng.UmengAppPushService;
import io.terminus.lib.apppush.impl.umeng.UmengToken;
import io.terminus.lib.email.EmailService;
import io.terminus.lib.email.impl.sendcloud.SendCloudEmailService;
import io.terminus.lib.email.impl.sendcloud.SendCloudToken;
import io.terminus.lib.sms.SmsService;
import io.terminus.lib.sms.impl.alibaba.AliSmsService;
import io.terminus.lib.sms.impl.alibaba.AliSmsToken;
import io.terminus.parana.config.ConfigCenter;
import io.terminus.parana.msg.component.MessagePostHandler;
import io.terminus.parana.msg.component.MessagePreHandler;
import io.terminus.parana.msg.component.MessageValidatorChain;
import io.terminus.parana.msg.component.handler.DefaultMessagePostHandler;
import io.terminus.parana.msg.component.handler.DefaultMessagePreHandler;
import io.terminus.parana.msg.component.validate.DefaultMessageValidatorChain;
import io.terminus.parana.web.msg.MsgGatewayBuilder;
import io.terminus.parana.web.msg.MsgWebService;
import io.terminus.parana.web.msg.impl.SmartMsgWebService;
import io.terminus.parana.web.msg.impl.common.DefaultMsgGatewayBuilder;
import io.terminus.parana.web.msg.mock.MockAppPushService;
import io.terminus.parana.web.msg.mock.MockEmailService;
import io.terminus.parana.web.msg.mock.MockMsgGatewayBuilder;
import io.terminus.parana.web.msg.mock.MockSmsService;
import io.terminus.parana.web.msg.test.AppPushController;
import io.terminus.parana.web.msg.test.EmailController;
import io.terminus.parana.web.msg.test.NotifyController;
import io.terminus.parana.web.msg.test.SmartMsgController;
import io.terminus.parana.web.msg.test.SmsController;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MsgWebConfig {
   @Bean
   public SmartMsgWebService smartMsgWebService() {
      return new SmartMsgWebService();
   }

   @Configuration
   @ConditionalOnBean({MsgGatewayBuilder.class})
   @ConditionalOnProperty(
      name = {"msg.webservice.version"},
      havingValue = "db"
   )
   public static class DbMsgWebServiceConfig {
      @Configuration
      @ConditionalOnProperty(
         name = {"msg.webservice.channel.apppush"}
      )
      @ComponentScan({"io.terminus.parana.web.msg.impl.apppush.common", "io.terminus.parana.web.msg.impl.apppush.db"})
      public static class DbAppPushConfig {
      }

      @Configuration
      @ComponentScan({"io.terminus.parana.web.msg.impl.common.db"})
      public static class DbCommonConfig {
      }

      @Configuration
      @ConditionalOnProperty(
         name = {"msg.webservice.channel.email"}
      )
      @ComponentScan({"io.terminus.parana.web.msg.impl.email.common", "io.terminus.parana.web.msg.impl.email.db"})
      public class DbEmailConfig {
      }

      @Configuration
      @ConditionalOnProperty(
         name = {"msg.webservice.channel.notify"}
      )
      @ComponentScan({"io.terminus.parana.web.msg.impl.notify.common", "io.terminus.parana.web.msg.impl.notify.db", "io.terminus.parana.web.msg.impl.notify.controller"})
      public class DbNotifyConfig {
      }

      @Configuration
      @ConditionalOnProperty(
         name = {"msg.webservice.channel.sms"}
      )
      @ComponentScan({"io.terminus.parana.web.msg.impl.sms.common", "io.terminus.parana.web.msg.impl.sms.db"})
      public class DbSmsConfig {
      }
   }

   @Configuration
   public static class MsgGateWayConfig {
      @ConditionalOnProperty(
         name = {"msg.gateway.version"},
         havingValue = "db"
      )
      public static class DbMsgGatewayConfig {
         @Bean
         public MsgGatewayBuilder dbMsgGatewayBuilder() {
            return new DefaultMsgGatewayBuilder();
         }

         @Bean
         @ConditionalOnProperty(
            name = {"msg.current.smsService"},
            havingValue = "aliSmsService"
         )
         public SmsService aliSmsService(ConfigCenter configCenter) {
            AliSmsToken token = new AliSmsToken();
            token.setAppKey((String)configCenter.get("msg.alisms.appKey").or("appKey"));
            token.setAppSecret((String)configCenter.get("msg.alisms.appSecret").or("appSecret"));
            token.setSmsUrl((String)configCenter.get("msg.alisms.url").or("http://gw.api.taobao.com/router/rest"));
            token.setVersion((String)configCenter.get("msg.alisms.version").or("2.0"));
            return new AliSmsService(token);
         }

         @Bean
         @ConditionalOnProperty(
            name = {"msg.current.emailService"},
            havingValue = "sendCloudEmailService"
         )
         public EmailService sendCloudEmailService(ConfigCenter configCenter) {
            SendCloudToken token = new SendCloudToken();
            token.setBatchUser((String)configCenter.get("msg.sendcloud.batchUser").or("batchUser"));
            token.setUser((String)configCenter.get("msg.sendcloud.user").or("user"));
            token.setTriggerUser((String)configCenter.get("msg.sendcloud.triggerUser").or("triggerUser"));
            token.setGateway((String)configCenter.get("msg.sendcloud.gateway").or("http://sendcloud.sohu.com/webapi/mail.send.json"));
            token.setKey((String)configCenter.get("msg.sendcloud.key").or("key"));
            return new SendCloudEmailService(token);
         }

         @Bean
         @ConditionalOnProperty(
            name = {"msg.current.appPushService"},
            havingValue = "umengAppPushService"
         )
         public AppPushService umengAppPushServiceAndroid(ConfigCenter configCenter) {
            UmengToken token = new UmengToken();
            token.setAppKey((String)configCenter.get("msg.umeng.android.appKey").or("defaultKey"));
            token.setAppMasterSecret((String)configCenter.get("msg.umeng.android.appSecret").or("defaultSecret"));
            token.setSendUrl((String)configCenter.get("msg.umeng.sendUrl").or("http://msg.umeng.com/api/send"));
            return new UmengAppPushService(token);
         }

         @Bean
         @ConditionalOnProperty(
            name = {"msg.current.appPushService"},
            havingValue = "umengAppPushService"
         )
         public AppPushService umengAppPushServiceIos(ConfigCenter configCenter) {
            UmengToken token = new UmengToken();
            token.setAppKey((String)configCenter.get("msg.umeng.ios.appKey").or("defaultKey"));
            token.setAppMasterSecret((String)configCenter.get("msg.umeng.ios.appSecret").or("defaultSecret"));
            token.setSendUrl((String)configCenter.get("msg.umeng.sendUrl").or("http://msg.umeng.com/api/send"));
            return new UmengAppPushService(token);
         }
      }

      @ConditionalOnProperty(
         name = {"msg.gateway.version"},
         havingValue = "mock"
      )
      public static class MockMsgGatewayConfig {
         @Bean
         public MsgGatewayBuilder mockMsgGatewayBuilder() {
            return new MockMsgGatewayBuilder();
         }

         @Bean
         public SmsService mockSmsService() {
            return new MockSmsService();
         }

         @Bean
         public EmailService mockEmailService() {
            return new MockEmailService();
         }

         @Bean
         public AppPushService mockAppPushService() {
            return new MockAppPushService();
         }
      }

      @ConditionalOnProperty(
         name = {"msg.gateway.version"},
         havingValue = "simple",
         matchIfMissing = true
      )
      public static class SimpleMsgGatewayConfig {
         @Bean
         public MsgGatewayBuilder simpleMsgGatewayBuilder() {
            return new DefaultMsgGatewayBuilder();
         }

         @Bean
         @ConditionalOnProperty(
            name = {"msg.current.smsService"},
            havingValue = "aliSmsService"
         )
         public SmsService aliSmsService(@Value("${msg.alisms.appKey:defaultKey}") String appKey, @Value("${msg.alisms.appSecret:defaultSecret}") String appSecret, @Value("${msg.alisms.smsUrl:http://gw.api.taobao.com/router/rest}") String smsUrl, @Value("${msg.alisms.version:2.0}") String version) {
            return new AliSmsService(appKey, appSecret, smsUrl, version);
         }

         @Bean
         @ConditionalOnProperty(
            name = {"msg.current.emailService"},
            havingValue = "sendCloudEmailService"
         )
         public EmailService sendCloudEmailService(@Value("${msg.sendcloud.user:user}") String user, @Value("${msg.sendcloud.triggerUser:triggerUser}") String triggerUser, @Value("${msg.sendcloud.batchUser:batchUser}") String batchUser, @Value("${msg.sendcloud.key:defaultkey}") String key, @Value("${msg.sendcloud.gateway:gateway}") String gateway) {
            return new SendCloudEmailService(user, triggerUser, batchUser, key, gateway);
         }

         @Bean
         @ConditionalOnProperty(
            name = {"msg.current.appPushService"},
            havingValue = "umengAppPushService"
         )
         public AppPushService umengAppPushServiceAndroid(@Value("${msg.umeng.android.appKey:defaultKey}") String appKey, @Value("${msg.umeng.android.appSecret:defaultSecret}") String appSecret) {
            return new UmengAppPushService(appKey, appSecret);
         }

         @Bean
         @ConditionalOnProperty(
            name = {"msg.current.appPushService"},
            havingValue = "umengAppPushService"
         )
         public AppPushService umengAppPushServiceIos(@Value("${msg.umeng.ios.appKey:defaultKey}") String appKey, @Value("${msg.umeng.ios.appSecret:defaultSecret}") String appSecret) {
            return new UmengAppPushService(appKey, appSecret);
         }
      }
   }

   @Configuration
   @ConditionalOnBean({MsgGatewayBuilder.class})
   @ConditionalOnProperty(
      name = {"msg.webservice.version"},
      havingValue = "simple"
   )
   public static class SimpleMsgWebServiceConfig {
      @Configuration
      @ConditionalOnProperty(
         name = {"msg.webservice.channel.apppush"}
      )
      @ComponentScan({"io.terminus.parana.web.msg.impl.apppush.common", "io.terminus.parana.web.msg.impl.apppush.simple"})
      public class SimpleAppPushConfig {
      }

      @Configuration
      @ComponentScan({"io.terminus.parana.web.msg.impl.common.simple"})
      public class SimpleCommonConfig {
         @Bean
         @ConditionalOnMissingBean({MessageValidatorChain.class})
         public MessageValidatorChain messageValidatorChain() {
            return new DefaultMessageValidatorChain();
         }

         @Bean
         @ConditionalOnMissingBean({MessagePreHandler.class})
         public MessagePreHandler messagePreHandler() {
            return new DefaultMessagePreHandler();
         }

         @Bean
         @ConditionalOnMissingBean({MessagePostHandler.class})
         public MessagePostHandler messagePostHandler() {
            return new DefaultMessagePostHandler();
         }
      }

      @Configuration
      @ConditionalOnProperty(
         name = {"msg.webservice.channel.email"}
      )
      @ComponentScan({"io.terminus.parana.web.msg.impl.email.common", "io.terminus.parana.web.msg.impl.email.simple"})
      public class SimpleEmailConfig {
      }

      @Configuration
      @ConditionalOnProperty(
         name = {"msg.webservice.channel.notify"}
      )
      @ComponentScan({"io.terminus.parana.web.msg.impl.notify.common", "io.terminus.parana.web.msg.impl.notify.controller", "io.terminus.parana.web.msg.impl.notify.simple"})
      public class SimpleNotifyConfig {
      }

      @Configuration
      @ConditionalOnProperty(
         name = {"msg.webservice.channel.sms"}
      )
      @ComponentScan({"io.terminus.parana.web.msg.impl.sms.common", "io.terminus.parana.web.msg.impl.sms.simple"})
      public class SimpleSmsConfig {
      }
   }

   @Configuration
   @ConditionalOnProperty(
      name = {"msg.test.enabled"},
      havingValue = "true"
   )
   public static class TestMsgWebServiceConfig {
      @Bean
      @ConditionalOnBean(
         name = {"appPushWebService"}
      )
      @ConditionalOnProperty(
         name = {"msg.webservice.channel.apppush"}
      )
      public AppPushController appPushController(@Qualifier("appPushWebService") MsgWebService appPushWebService) {
         return new AppPushController(appPushWebService);
      }

      @Bean
      @ConditionalOnBean(
         name = {"emailWebService"}
      )
      @ConditionalOnProperty(
         name = {"msg.webservice.channel.email"}
      )
      public EmailController emailController(@Qualifier("emailWebService") MsgWebService emailWebService) {
         return new EmailController(emailWebService);
      }

      @Bean
      @ConditionalOnBean(
         name = {"notifyWebService"}
      )
      @ConditionalOnProperty(
         name = {"msg.webservice.channel.notify"}
      )
      public NotifyController notifyController(@Qualifier("notifyWebService") MsgWebService notifyWebService) {
         return new NotifyController(notifyWebService);
      }

      @Bean
      @ConditionalOnBean(
         name = {"smsWebService"}
      )
      @ConditionalOnProperty(
         name = {"msg.webservice.channel.sms"}
      )
      public SmsController smsController(@Qualifier("smsWebService") MsgWebService smsWebService) {
         return new SmsController(smsWebService);
      }

      @Bean
      public SmartMsgController smartMsgController(SmartMsgWebService smartMsgWebService) {
         return new SmartMsgController(smartMsgWebService);
      }
   }
}
