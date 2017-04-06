package cn.blmdz.wolf.msg.component.handler;

import com.google.common.base.Strings;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.wolf.msg.component.MessagePreHandler;
import cn.blmdz.wolf.msg.model.Message;

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
