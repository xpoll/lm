package io.terminus.parana.web.msg.mock;

import com.google.common.base.Throwables;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.lib.apppush.AppPushService;
import io.terminus.lib.email.EmailService;
import io.terminus.lib.sms.SmsService;
import io.terminus.parana.web.msg.MsgGatewayBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

public class MockMsgGatewayBuilder implements MsgGatewayBuilder, ApplicationContextAware {
   private static final Logger log = LoggerFactory.getLogger(MockMsgGatewayBuilder.class);
   private ApplicationContext applicationContext;

   public String getCurrentEmailService() {
      return "mockEmailService";
   }

   public String getCurrentSmsService() {
      return "mockSmsService";
   }

   public String getCurrentAppPushService() {
      return "mockAppPushService";
   }

   public EmailService buildEmailService() {
      try {
         return (EmailService)this.applicationContext.getBean(this.getCurrentEmailService());
      } catch (Exception var2) {
         log.error("buildEmailService failed, cause={}", Throwables.getStackTraceAsString(var2));
         throw new JsonResponseException("email.service.not.exists");
      }
   }

   public SmsService buildSmsService() {
      try {
         return (SmsService)this.applicationContext.getBean(this.getCurrentSmsService());
      } catch (Exception var2) {
         log.error("buildSmsService failed, cause={}", Throwables.getStackTraceAsString(var2));
         throw new JsonResponseException("sms.service.not.exists");
      }
   }

   public AppPushService buildAppPushService(String deviceType) {
      try {
         return (AppPushService)this.applicationContext.getBean(this.getCurrentAppPushService());
      } catch (Exception var3) {
         log.error("buildAppPushService failed, cause={}", Throwables.getStackTraceAsString(var3));
         throw new JsonResponseException("app.push.service.not.exists");
      }
   }

   public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
      this.applicationContext = applicationContext;
   }
}
