package cn.blmdz.wolf.msg.component.handler;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.wolf.msg.component.MessagePostHandler;
import cn.blmdz.wolf.msg.model.Message;

public class DefaultMessagePostHandler implements MessagePostHandler {
   public void postHandle(Message message) throws ServiceException {
      message.setStatus(Integer.valueOf(Message.Status.Initial.value()));
      if(message.getCheckSubscribe() == null) {
         message.setCheckSubscribe(Boolean.valueOf(false));
      }

   }
}
