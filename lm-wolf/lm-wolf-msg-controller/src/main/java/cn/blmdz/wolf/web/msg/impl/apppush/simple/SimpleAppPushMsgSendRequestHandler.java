package cn.blmdz.wolf.web.msg.impl.apppush.simple;

import java.util.Map;
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
public class SimpleAppPushMsgSendRequestHandler extends SimpleMsgSendRequestHandler implements MsgSendRequestHandler {
   @Autowired
   public SimpleAppPushMsgSendRequestHandler(MessagePreHandler messagePreHandler, MessagePostHandler messagePostHandler, MessageValidatorChain messageValidatorChain) {
      super(messagePreHandler, messagePostHandler, messageValidatorChain);
   }

   public Message request(String toes, String templateName, Map context, String extra) {
      return super.request(Integer.valueOf(Channel.AppPush.value()), toes, templateName, context, extra);
   }

   public Message request(String toes, String title, String content, String extra) {
      return super.request(Integer.valueOf(Channel.AppPush.value()), toes, title, content, extra);
   }

   public Message request(Message message) {
      message.setChannel(Integer.valueOf(Channel.AppPush.value()));
      return super.request(message);
   }
}
