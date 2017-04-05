package io.terminus.parana.web.msg.impl.common.db;

import io.terminus.common.exception.JsonResponseException;
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
import io.terminus.parana.web.msg.MsgGatewayBuilder;
import org.springframework.beans.factory.annotation.Autowired;

public class DbMsgGatewayBuilder implements MsgGatewayBuilder {
   protected final ConfigCenter configCenter;

   @Autowired
   public DbMsgGatewayBuilder(ConfigCenter configCenter) {
      this.configCenter = configCenter;
   }

   public String getCurrentEmailService() {
      return (String)this.configCenter.get("msg.current.email.service").or("sendCloudEmailService");
   }

   public String getCurrentSmsService() {
      return (String)this.configCenter.get("msg.current.sms.service").or("aliSmsService");
   }

   public String getCurrentAppPushService() {
      return (String)this.configCenter.get("msg.current.app.push.service").or("umengAppPushService");
   }

   public EmailService buildEmailService() {
      String emailService = this.getCurrentEmailService();
      if(emailService.equals("sendCloudEmailService")) {
         return new SendCloudEmailService(this.getSendCloudToken());
      } else {
         throw new JsonResponseException("email.service.not.exists");
      }
   }

   public SmsService buildSmsService() {
      String smsService = this.getCurrentSmsService();
      if(smsService.equals("aliSmsService")) {
         return new AliSmsService(this.getAliSmsToken());
      } else {
         throw new JsonResponseException("sms.service.not.exists");
      }
   }

   public AppPushService buildAppPushService(String deviceType) {
      String appPushService = this.getCurrentAppPushService();
      if(appPushService.equals("umengAppPushService")) {
         if(deviceType.equals("android")) {
            return new UmengAppPushService(this.getUmengTokenAndroid());
         } else if(deviceType.equals("ios")) {
            return new UmengAppPushService(this.getUmengTokenIos());
         } else {
            throw new JsonResponseException("app.push.service.not.exits");
         }
      } else {
         throw new JsonResponseException("app.push.service.not.exits");
      }
   }

   protected UmengToken getUmengTokenAndroid() {
      UmengToken token = new UmengToken();
      token.setAppKey((String)this.configCenter.get("msg.umeng.android.appKey").or("defaultKey"));
      token.setAppMasterSecret((String)this.configCenter.get("msg.umeng.android.appSecret").or("defaultSecret"));
      token.setSendUrl((String)this.configCenter.get("msg.umeng.sendUrl").or("http://msg.umeng.com/api/send"));
      return token;
   }

   protected UmengToken getUmengTokenIos() {
      UmengToken token = new UmengToken();
      token.setAppKey((String)this.configCenter.get("msg.umeng.ios.appKey").or("defaultKey"));
      token.setAppMasterSecret((String)this.configCenter.get("msg.umeng.ios.appSecret").or("defaultSecret"));
      token.setSendUrl((String)this.configCenter.get("msg.umeng.sendUrl").or("http://msg.umeng.com/api/send"));
      return token;
   }

   protected SendCloudToken getSendCloudToken() {
      SendCloudToken token = new SendCloudToken();
      token.setBatchUser((String)this.configCenter.get("msg.sendcloud.batchUser").or("batchUser"));
      token.setUser((String)this.configCenter.get("msg.sendcloud.user").or("user"));
      token.setTriggerUser((String)this.configCenter.get("msg.sendcloud.triggerUser").or("triggerUser"));
      token.setGateway((String)this.configCenter.get("msg.sendcloud.gateway").or("http://sendcloud.sohu.com/webapi/mail.send.json"));
      token.setKey((String)this.configCenter.get("msg.sendcloud.key").or("key"));
      return token;
   }

   protected AliSmsToken getAliSmsToken() {
      AliSmsToken token = new AliSmsToken();
      token.setAppKey((String)this.configCenter.get("msg.alisms.appKey").or("appKey"));
      token.setAppSecret((String)this.configCenter.get("msg.alisms.appSecret").or("appSecret"));
      token.setSmsUrl((String)this.configCenter.get("msg.alisms.url").or("http://gw.api.taobao.com/router/rest"));
      token.setVersion((String)this.configCenter.get("msg.alisms.version").or("2.0"));
      return token;
   }
}
