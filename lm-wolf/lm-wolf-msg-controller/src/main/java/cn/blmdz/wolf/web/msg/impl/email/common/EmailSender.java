package cn.blmdz.wolf.web.msg.impl.email.common;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.aide.email.EmailService;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.model.Message;
import cn.blmdz.wolf.web.msg.MsgGatewayBuilder;
import cn.blmdz.wolf.web.msg.MsgSender;

@Component
public class EmailSender implements MsgSender {
   private static final Logger log = LoggerFactory.getLogger(EmailSender.class);
   private final MsgGatewayBuilder msgGatewayBuilder;

   @Autowired
   public EmailSender(MsgGatewayBuilder msgGatewayBuilder) {
      this.msgGatewayBuilder = msgGatewayBuilder;
   }

   public Response send(Message message) {
      try {
         EmailService emailService = this.msgGatewayBuilder.buildEmailService();
         String result = emailService.send(message.getTitle(), message.getContent(), message.getReceivers(), message.getAttaches());
         return Response.ok(result);
      } catch (Exception var4) {
         return Response.fail(var4.getMessage());
      }
   }
}
