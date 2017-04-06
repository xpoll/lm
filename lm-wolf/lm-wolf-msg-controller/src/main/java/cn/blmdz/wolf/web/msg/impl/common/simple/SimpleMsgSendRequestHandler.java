package cn.blmdz.wolf.web.msg.impl.common.simple;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.msg.component.MessagePostHandler;
import cn.blmdz.wolf.msg.component.MessagePreHandler;
import cn.blmdz.wolf.msg.component.MessageValidatorChain;
import cn.blmdz.wolf.msg.model.Message;

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
