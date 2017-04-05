package io.terminus.parana.msg.component.validate;

import io.terminus.common.exception.ServiceException;
import io.terminus.common.utils.JsonMapper;
import io.terminus.parana.msg.component.MessageValidator;
import io.terminus.parana.msg.component.validate.CommonPattern;
import io.terminus.parana.msg.model.Channel;
import io.terminus.parana.msg.model.Message;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
