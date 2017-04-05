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

public class SmsValidator implements MessageValidator {
   private static final Logger log = LoggerFactory.getLogger(SmsValidator.class);

   public void check(Message message) throws ServiceException {
      if(message.getChannel().intValue() == Channel.Sms.value()) {
         List<String> mobiles = (List)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(message.getReceivers(), JsonMapper.JSON_NON_EMPTY_MAPPER.createCollectionType(List.class, new Class[]{String.class}));
         if(mobiles == null) {
            log.error("validate message receivers mobile failed, receivers={}", message.getReceivers());
            throw new ServiceException("message.receivers.mobile.invalid");
         } else {
            for(String mobile : mobiles) {
               if(CommonPattern.MOBILE.invalid(mobile)) {
                  log.error("validate message receivers mobile failed, receivers={}", message.getReceivers());
                  throw new ServiceException("message.receivers.mobile.invalid");
               }
            }

         }
      }
   }
}
