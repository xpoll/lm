package io.terminus.parana.msg.component.handler;

import io.terminus.common.exception.ServiceException;
import io.terminus.parana.msg.component.MessagePostHandler;
import io.terminus.parana.msg.model.Message;

public class DefaultMessagePostHandler implements MessagePostHandler {
   public void postHandle(Message message) throws ServiceException {
      message.setStatus(Integer.valueOf(Message.Status.Initial.value()));
      if(message.getCheckSubscribe() == null) {
         message.setCheckSubscribe(Boolean.valueOf(false));
      }

   }
}
