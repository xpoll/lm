package io.terminus.parana.web.msg.impl.common;

import com.google.common.base.Throwables;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.lib.apppush.AppPushService;
import io.terminus.lib.email.EmailService;
import io.terminus.lib.sms.SmsService;
import io.terminus.parana.web.msg.MsgGatewayBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

public class DefaultMsgGatewayBuilder implements MsgGatewayBuilder, ApplicationContextAware {
   private static final Logger log = LoggerFactory.getLogger(DefaultMsgGatewayBuilder.class);
   @Value("${msg.current.emailService:sendCloudEmailService}")
   private String currentEmailService;
   @Value("${msg.current.smsService:aliSmsService}")
   private String currentSmsService;
   @Value("${msg.current.appPushService:umengAppPushService}")
   private String currentAppPushService;
   private ApplicationContext applicationContext;

   public String getCurrentEmailService() {
      return this.currentEmailService;
   }

   public String getCurrentSmsService() {
      return this.currentSmsService;
   }

   public String getCurrentAppPushService() {
      return this.currentAppPushService;
   }

   public EmailService buildEmailService() {
      try {
         return (EmailService)this.applicationContext.getBean(this.currentEmailService);
      } catch (Exception var2) {
         log.error("buildEmailService failed, cause={}", Throwables.getStackTraceAsString(var2));
         throw new JsonResponseException("email.service.not.exists");
      }
   }

   public SmsService buildSmsService() {
      try {
         return (SmsService)this.applicationContext.getBean(this.currentSmsService);
      } catch (Exception var2) {
         log.error("buildSmsService failed, cause={}", Throwables.getStackTraceAsString(var2));
         throw new JsonResponseException("sms.service.not.exists");
      }
   }

   public AppPushService buildAppPushService(String deviceType) {
      try {
         if(deviceType.equals("android")) {
            return (AppPushService)this.applicationContext.getBean(this.currentAppPushService + "Android");
         } else if(deviceType.equals("ios")) {
            return (AppPushService)this.applicationContext.getBean(this.currentAppPushService + "Ios");
         } else {
            throw new JsonResponseException("not.supported.device.type");
         }
      } catch (Exception var3) {
         log.error("buildAppPushService failed, cause={}", Throwables.getStackTraceAsString(var3));
         throw new JsonResponseException("app.push.service.not.exists");
      }
   }

   public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
      this.applicationContext = applicationContext;
   }
}
