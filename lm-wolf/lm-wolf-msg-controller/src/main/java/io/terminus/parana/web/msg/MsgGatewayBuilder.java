package io.terminus.parana.web.msg;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.lib.apppush.AppPushService;
import io.terminus.lib.email.EmailService;
import io.terminus.lib.sms.SmsService;

public interface MsgGatewayBuilder {
   String getCurrentEmailService();

   String getCurrentSmsService();

   String getCurrentAppPushService();

   SmsService buildSmsService() throws JsonResponseException;

   EmailService buildEmailService() throws JsonResponseException;

   AppPushService buildAppPushService(String var1) throws JsonResponseException;
}
