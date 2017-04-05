package io.terminus.parana.msg.component.validate;

import io.terminus.common.exception.ServiceException;
import io.terminus.common.utils.JsonMapper;
import io.terminus.parana.msg.component.MessageValidator;
import io.terminus.parana.msg.dto.AppPushReceivers;
import io.terminus.parana.msg.model.Channel;
import io.terminus.parana.msg.model.Message;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AppPushValidator implements MessageValidator {
   private static final Logger log = LoggerFactory.getLogger(AppPushValidator.class);

   public void check(Message message) throws ServiceException {
      if(message.getChannel().intValue() == Channel.AppPush.value()) {
         boolean legal = true;
         AppPushReceivers deviceTokens = (AppPushReceivers)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(message.getReceivers(), AppPushReceivers.class);
         if(deviceTokens == null) {
            legal = false;
         } else if(deviceTokens.getAndroid().size() + deviceTokens.getIos().size() + deviceTokens.getWp().size() < 1) {
            legal = false;
         }

         if(!legal) {
            log.error("validate message receivers deviceTokens failed, receivers={}", message.getReceivers());
            throw new ServiceException("message.receivers.device.tokens.invalid");
         }
      }
   }
}
