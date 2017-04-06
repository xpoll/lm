package cn.blmdz.wolf.msg.component.validate;

import com.google.common.base.Strings;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.wolf.msg.component.MessageValidator;
import cn.blmdz.wolf.msg.model.Message;

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
