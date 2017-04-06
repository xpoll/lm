package cn.blmdz.wolf.web.msg.impl.sms.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.aide.sms.SmsService;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.model.Message;
import cn.blmdz.wolf.web.msg.MsgGatewayBuilder;
import cn.blmdz.wolf.web.msg.MsgSender;

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
