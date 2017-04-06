package cn.blmdz.wolf.web.msg.mock;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import com.google.common.base.Throwables;

import cn.blmdz.aide.apppush.AppPushService;
import cn.blmdz.aide.email.EmailService;
import cn.blmdz.aide.sms.SmsService;
import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.wolf.web.msg.MsgGatewayBuilder;

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
