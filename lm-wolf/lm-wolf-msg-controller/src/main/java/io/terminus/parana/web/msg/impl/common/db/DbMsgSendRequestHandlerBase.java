package io.terminus.parana.web.msg.impl.common.db;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.parana.msg.model.Message;
import io.terminus.parana.msg.service.MessageReadService;
import io.terminus.parana.msg.service.MessageWriteService;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DbMsgSendRequestHandlerBase {
   private static final Logger log = LoggerFactory.getLogger(DbMsgSendRequestHandlerBase.class);
   protected final MessageWriteService messageWriteService;
   protected final MessageReadService messageReadService;

   public DbMsgSendRequestHandlerBase(MessageReadService messageReadService, MessageWriteService messageWriteService) {
      this.messageReadService = messageReadService;
      this.messageWriteService = messageWriteService;
   }

   public Message request(Integer channel, String toes, String templateName, Map context, String extra) {
      Response<Long> response = this.messageWriteService.sendMessageViaTemplate(channel, templateName, context, toes, extra);
      if(!response.isSuccess()) {
         log.error("sendMessageViaTemplate failed, cause={}", response.getError());
         throw new JsonResponseException(response.getError());
      } else {
         Response<Message> readResponse = this.messageReadService.findMessageById((Long)response.getResult());
         if(!readResponse.isSuccess()) {
            log.error("findMessageById failed, messageId={}, cause={}", response.getResult(), readResponse.getError());
            throw new JsonResponseException(response.getError());
         } else {
            return (Message)readResponse.getResult();
         }
      }
   }

   public Message request(Integer channel, String toes, String title, String content, String extra) {
      Response<Long> response = this.messageWriteService.sendMessage(channel, title, content, toes, extra);
      if(!response.isSuccess()) {
         log.error("sendMessage failed, cause={}", response.getError());
         throw new JsonResponseException(response.getError());
      } else {
         Response<Message> readResponse = this.messageReadService.findMessageById((Long)response.getResult());
         if(!readResponse.isSuccess()) {
            log.error("findMessageById failed, messageId={}, cause={}", response.getResult(), readResponse.getError());
            throw new JsonResponseException(response.getError());
         } else {
            return (Message)readResponse.getResult();
         }
      }
   }

   public Message request(Message message) {
      Response<Long> response = this.messageWriteService.createMessage(message);
      if(!response.isSuccess()) {
         log.error("createMessage failed, cause={}", response.getError());
         throw new JsonResponseException(response.getError());
      } else {
         Response<Message> readResponse = this.messageReadService.findMessageById((Long)response.getResult());
         if(!readResponse.isSuccess()) {
            log.error("findMessageById failed, messageId={}, cause={}", response.getResult(), readResponse.getError());
            throw new JsonResponseException(response.getError());
         } else {
            return (Message)readResponse.getResult();
         }
      }
   }
}
