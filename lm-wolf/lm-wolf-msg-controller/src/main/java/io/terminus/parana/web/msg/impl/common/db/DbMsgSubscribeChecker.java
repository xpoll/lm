package io.terminus.parana.web.msg.impl.common.db;

import io.terminus.common.model.Response;
import io.terminus.parana.msg.model.Channel;
import io.terminus.parana.msg.model.Message;
import io.terminus.parana.msg.service.MessageReadService;
import io.terminus.parana.web.msg.MsgSubscribeChecker;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DbMsgSubscribeChecker implements MsgSubscribeChecker {
   private static final Logger log = LoggerFactory.getLogger(DbMsgSubscribeChecker.class);
   private final MessageReadService messageReadService;

   @Autowired
   public DbMsgSubscribeChecker(MessageReadService messageReadService) {
      this.messageReadService = messageReadService;
   }

   public void checkSubscribe(Message message) {
      if(Objects.equals(message.getChannel(), Integer.valueOf(Channel.Email.value())) && message.getCheckSubscribe() != null && message.getCheckSubscribe().booleanValue()) {
         Response<String> response = this.messageReadService.getMessageActualReceivers(message.getReceivers());
         String messageReceivers;
         if(response.isSuccess()) {
            messageReceivers = (String)response.getResult();
         } else {
            log.error("getMessageActualReceivers failed, message={}, cause={}", message, response.getError());
            messageReceivers = message.getReceivers();
         }

         message.setReceivers(messageReceivers);
      }

   }
}
