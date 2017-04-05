package io.terminus.parana.msg.component.handler;

import com.google.common.base.Strings;
import io.terminus.common.exception.ServiceException;
import io.terminus.parana.msg.component.MessagePreHandler;
import io.terminus.parana.msg.model.Message;

public class DefaultMessagePreHandler implements MessagePreHandler {
   public void preHandle(Message message) throws ServiceException {
      if(Strings.isNullOrEmpty(message.getTitle())) {
         message.setTitle("message");
      }

      if(message.getGroupMessageType() == null) {
         message.setGroupMessageType(Integer.valueOf(Message.GroupMessageType.NotGroupMessage.value()));
      }

      if(message.getGroupMessageType().equals(Integer.valueOf(Message.GroupMessageType.NotGroupMessage.value())) && !message.getReceivers().contains("[") && !message.getReceivers().contains("{")) {
         message.setReceivers("[\"" + message.getReceivers() + "\"]");
      }

   }
}
