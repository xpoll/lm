package io.terminus.parana.web.msg.impl.email.common;

import io.terminus.common.model.Response;
import io.terminus.lib.email.EmailService;
import io.terminus.parana.msg.model.Message;
import io.terminus.parana.web.msg.MsgGatewayBuilder;
import io.terminus.parana.web.msg.MsgSender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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
