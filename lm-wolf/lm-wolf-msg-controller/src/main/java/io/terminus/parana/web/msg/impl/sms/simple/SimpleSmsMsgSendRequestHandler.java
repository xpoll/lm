package io.terminus.parana.web.msg.impl.sms.simple;

import io.terminus.parana.msg.component.MessagePostHandler;
import io.terminus.parana.msg.component.MessagePreHandler;
import io.terminus.parana.msg.component.MessageValidatorChain;
import io.terminus.parana.msg.model.Channel;
import io.terminus.parana.msg.model.Message;
import io.terminus.parana.web.msg.MsgSendRequestHandler;
import io.terminus.parana.web.msg.impl.common.simple.SimpleMsgSendRequestHandler;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SimpleSmsMsgSendRequestHandler extends SimpleMsgSendRequestHandler implements MsgSendRequestHandler {
   @Autowired
   public SimpleSmsMsgSendRequestHandler(MessagePreHandler messagePreHandler, MessagePostHandler messagePostHandler, MessageValidatorChain messageValidatorChain) {
      super(messagePreHandler, messagePostHandler, messageValidatorChain);
   }

   public Message request(String toes, String templateName, Map context, String extra) {
      return super.request(Integer.valueOf(Channel.Sms.value()), toes, templateName, context, extra);
   }

   public Message request(String toes, String title, String content, String extra) {
      return super.request(Integer.valueOf(Channel.Sms.value()), toes, title, content, extra);
   }

   public Message request(Message message) {
      message.setChannel(Integer.valueOf(Channel.Sms.value()));
      return super.request(message);
   }
}
