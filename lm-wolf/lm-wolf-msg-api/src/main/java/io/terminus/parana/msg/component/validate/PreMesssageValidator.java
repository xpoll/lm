package io.terminus.parana.msg.component.validate;

import com.google.common.base.Strings;
import io.terminus.common.exception.ServiceException;
import io.terminus.parana.msg.component.MessageValidator;
import io.terminus.parana.msg.model.Message;

public class PreMesssageValidator implements MessageValidator {
   public void check(Message message) throws ServiceException {
      if(message.getChannel() == null) {
         throw new ServiceException("message.channel.required");
      } else if(Strings.isNullOrEmpty(message.getReceivers())) {
         throw new ServiceException("message.receivers.required");
      } else if(Strings.isNullOrEmpty(message.getTemplate()) && Strings.isNullOrEmpty(message.getContent())) {
         throw new ServiceException("message.template.or.content.required.one");
      }
   }
}
