package cn.blmdz.wolf.web.msg;

import cn.blmdz.aide.apppush.AppPushService;
import cn.blmdz.aide.email.EmailService;
import cn.blmdz.aide.sms.SmsService;
import cn.blmdz.home.common.exception.JsonResponseException;

public interface MsgGatewayBuilder {
   String getCurrentEmailService();

   String getCurrentSmsService();

   String getCurrentAppPushService();

   SmsService buildSmsService() throws JsonResponseException;

   EmailService buildEmailService() throws JsonResponseException;

   AppPushService buildAppPushService(String var1) throws JsonResponseException;
}
