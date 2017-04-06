package cn.blmdz.wolf.web.msg.impl.email.simple;

import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.wolf.msg.component.MessagePostHandler;
import cn.blmdz.wolf.msg.component.MessagePreHandler;
import cn.blmdz.wolf.msg.component.MessageValidatorChain;
import cn.blmdz.wolf.msg.model.Channel;
import cn.blmdz.wolf.msg.model.Message;
import cn.blmdz.wolf.web.msg.MsgSendRequestHandler;
import cn.blmdz.wolf.web.msg.impl.common.simple.SimpleMsgSendRequestHandler;

@Component
public class SimpleEmailMsgSendRequestHandler extends SimpleMsgSendRequestHandler implements MsgSendRequestHandler {
   private static final Logger log = LoggerFactory.getLogger(SimpleEmailMsgSendRequestHandler.class);

   @Autowired
   public SimpleEmailMsgSendRequestHandler(MessagePreHandler messagePreHandler, MessagePostHandler messagePostHandler, MessageValidatorChain messageValidatorChain) {
      super(messagePreHandler, messagePostHandler, messageValidatorChain);
   }

   public Message request(String toes, String templateName, Map context, String extra) {
      return super.request(Integer.valueOf(Channel.Email.value()), toes, templateName, context, extra);
   }

   public Message request(String toes, String title, String content, String extra) {
      return super.request(Integer.valueOf(Channel.Email.value()), toes, title, content, extra);
   }

   public Message request(Message message) {
      message.setChannel(Integer.valueOf(Channel.Email.value()));
      return super.request(message);
   }
}
