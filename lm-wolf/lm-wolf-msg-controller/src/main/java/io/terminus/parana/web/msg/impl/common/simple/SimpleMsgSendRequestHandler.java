package io.terminus.parana.web.msg.impl.common.simple;

import io.terminus.common.utils.JsonMapper;
import io.terminus.parana.msg.component.MessagePostHandler;
import io.terminus.parana.msg.component.MessagePreHandler;
import io.terminus.parana.msg.component.MessageValidatorChain;
import io.terminus.parana.msg.model.Message;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SimpleMsgSendRequestHandler {
   private static final Logger log = LoggerFactory.getLogger(SimpleMsgSendRequestHandler.class);
   private final MessagePreHandler messagePreHandler;
   private final MessagePostHandler messagePostHandler;
   private final MessageValidatorChain messageValidatorChain;

   @Autowired
   public SimpleMsgSendRequestHandler(MessagePreHandler messagePreHandler, MessagePostHandler messagePostHandler, MessageValidatorChain messageValidatorChain) {
      this.messagePreHandler = messagePreHandler;
      this.messagePostHandler = messagePostHandler;
      this.messageValidatorChain = messageValidatorChain;
   }

   public Message request(Integer channel, String toes, String templateName, Map context, String extra) {
      Message message = (Message)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(extra, Message.class);
      message.setReceivers(toes);
      message.setTemplate(templateName);
      message.setChannel(channel);
      message.setData(JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(context));
      return this.doHandleMessage(message);
   }

   public Message request(Integer channel, String toes, String title, String content, String extra) {
      Message message = (Message)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(extra, Message.class);
      message.setReceivers(toes);
      message.setContent(content);
      message.setChannel(channel);
      message.setTitle(title);
      return this.doHandleMessage(message);
   }

   public Message request(Message message) {
      return this.doHandleMessage(message);
   }

   public Message doHandleMessage(Message message) {
      this.messagePreHandler.preHandle(message);
      this.messageValidatorChain.check(message);
      this.messagePostHandler.postHandle(message);
      log.info("request send message={}", message);
      return message;
   }
}
