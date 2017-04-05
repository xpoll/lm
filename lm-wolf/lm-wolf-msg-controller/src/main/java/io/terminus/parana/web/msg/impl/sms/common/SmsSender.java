package io.terminus.parana.web.msg.impl.sms.common;

import io.terminus.common.model.Response;
import io.terminus.lib.sms.SmsService;
import io.terminus.parana.msg.model.Message;
import io.terminus.parana.web.msg.MsgGatewayBuilder;
import io.terminus.parana.web.msg.MsgSender;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SmsSender implements MsgSender {
   private final MsgGatewayBuilder msgGatewayBuilder;

   @Autowired
   public SmsSender(MsgGatewayBuilder msgGatewayBuilder) {
      this.msgGatewayBuilder = msgGatewayBuilder;
   }

   public Response send(Message message) {
      try {
         SmsService smsService = this.msgGatewayBuilder.buildSmsService();
         String result = smsService.send(message.getSender(), message.getReceivers(), message.getContent());
         return Response.ok(result);
      } catch (Exception var4) {
         return Response.fail(var4.getMessage());
      }
   }
}
