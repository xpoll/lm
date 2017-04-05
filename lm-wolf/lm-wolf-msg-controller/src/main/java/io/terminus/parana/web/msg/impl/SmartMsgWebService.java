package io.terminus.parana.web.msg.impl;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.parana.msg.model.Channel;
import io.terminus.parana.msg.model.Message;
import io.terminus.parana.web.msg.MsgWebService;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class SmartMsgWebService {
   private static final Logger log = LoggerFactory.getLogger(SmartMsgWebService.class);
   @Autowired(
      required = false
   )
   @Qualifier("emailWebService")
   private MsgWebService emailWebService;
   @Autowired(
      required = false
   )
   @Qualifier("appPushWebService")
   private MsgWebService appPushWebService;
   @Autowired(
      required = false
   )
   @Qualifier("smsWebService")
   private MsgWebService smsWebService;
   @Autowired(
      required = false
   )
   @Qualifier("notifyWebService")
   private MsgWebService notifyWebService;

   public String send(String toes, String templateName, Map context, String extra) throws JsonResponseException {
      Channel channel = this.detectChannel(toes);
      switch(channel) {
      case AppPush:
         this.existAppPushWebService();
         return this.appPushWebService.send(toes, templateName, context, extra);
      case Sms:
         this.existSmsWebService();
         return this.smsWebService.send(toes, templateName, context, extra);
      case Email:
         this.existEmailWebService();
         return this.emailWebService.send(toes, templateName, context, extra);
      case Notification:
         this.existNotifyWebService();
         return this.notifyWebService.send(toes, templateName, context, extra);
      default:
         log.error("toes format invalid, toes={}", toes);
         throw new JsonResponseException("toes.format.invalid");
      }
   }

   public String send(String toes, String title, String content, String extra) throws JsonResponseException {
      Channel channel = this.detectChannel(toes);
      switch(channel) {
      case AppPush:
         this.existAppPushWebService();
         return this.appPushWebService.send(toes, title, content, extra);
      case Sms:
         this.existSmsWebService();
         return this.smsWebService.send(toes, title, content, extra);
      case Email:
         this.existEmailWebService();
         return this.emailWebService.send(toes, title, content, extra);
      case Notification:
         this.existNotifyWebService();
         return this.notifyWebService.send(toes, title, content, extra);
      default:
         log.error("toes format invalid, toes={}", toes);
         throw new JsonResponseException("toes.format.invalid");
      }
   }

   public String send(Message message) throws JsonResponseException {
      Channel channel = this.detectChannel(message.getReceivers());
      switch(channel) {
      case AppPush:
         this.existAppPushWebService();
         return this.appPushWebService.send(message);
      case Sms:
         this.existSmsWebService();
         return this.smsWebService.send(message);
      case Email:
         this.existEmailWebService();
         return this.emailWebService.send(message);
      case Notification:
         this.existNotifyWebService();
         return this.notifyWebService.send(message);
      default:
         log.error("toes format invalid, toes={}", message.getReceivers());
         throw new JsonResponseException("toes.format.invalid");
      }
   }

   public String sendAppMessageByUserId(String toes, String templateName, Map context, String extra) throws JsonResponseException {
      this.existAppPushWebService();
      return this.appPushWebService.send(toes, templateName, context, extra);
   }

   public String sendAppMessageByUserId(String toes, String title, String content, String extra) throws JsonResponseException {
      this.existAppPushWebService();
      return this.appPushWebService.send(toes, title, content, extra);
   }

   public String sendAppMessageByUserId(Message message) throws JsonResponseException {
      this.existAppPushWebService();
      return this.appPushWebService.send(message);
   }

   protected void existAppPushWebService() throws JsonResponseException {
      if(this.appPushWebService == null) {
         log.error("appPushWebService bean missing");
         throw new JsonResponseException("appPushWebService.bean.missing");
      }
   }

   protected void existEmailWebService() throws JsonResponseException {
      if(this.emailWebService == null) {
         log.error("emailWebService bean missing");
         throw new JsonResponseException("emailWebService.bean.missing");
      }
   }

   protected void existNotifyWebService() throws JsonResponseException {
      if(this.notifyWebService == null) {
         log.error("notifyWebService bean missing");
         throw new JsonResponseException("notifyWebService.bean.missing");
      }
   }

   protected void existSmsWebService() throws JsonResponseException {
      if(this.smsWebService == null) {
         log.error("smsWebService bean missing");
         throw new JsonResponseException("smsWebService.bean.missing");
      }
   }

   protected Channel detectChannel(String toes) {
      if(toes.contains("@")) {
         return Channel.Email;
      } else if(!toes.contains("[") && !toes.contains("{")) {
         return Channel.Notification;
      } else if(toes.trim().startsWith("[")) {
         return !toes.contains("\"")?Channel.Notification:Channel.Sms;
      } else {
         if(toes.trim().startsWith("{")) {
            if(toes.contains("android") || toes.contains("ios") || toes.contains("wp")) {
               return Channel.AppPush;
            }

            if(toes.contains("group")) {
               return Channel.Notification;
            }
         }

         return Channel.Unknown;
      }
   }
}
