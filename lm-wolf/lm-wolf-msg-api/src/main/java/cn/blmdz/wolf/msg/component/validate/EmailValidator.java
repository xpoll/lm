package cn.blmdz.wolf.msg.component.validate;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.msg.component.MessageValidator;
import cn.blmdz.wolf.msg.model.Channel;
import cn.blmdz.wolf.msg.model.Message;

public class EmailValidator implements MessageValidator {
   private static final Logger log = LoggerFactory.getLogger(EmailValidator.class);

   public void check(Message message) {
      if(message.getChannel().intValue() == Channel.Email.value()) {
         List<String> emails = (List)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(message.getReceivers(), JsonMapper.JSON_NON_EMPTY_MAPPER.createCollectionType(List.class, new Class[]{String.class}));
         if(emails == null) {
            log.error("validate message receivers email failed, receivers={}", message.getReceivers());
            throw new ServiceException("message.receivers.email.invalid");
         } else {
            for(String email : emails) {
               if(CommonPattern.EMAIL.invalid(email)) {
                  log.error("validate message receivers email failed, receivers={}", message.getReceivers());
                  throw new ServiceException("message.receivers.email.invalid");
               }
            }

         }
      }
   }
}
