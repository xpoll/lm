package cn.blmdz.wolf.msg.component.validate;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.msg.component.MessageValidator;
import cn.blmdz.wolf.msg.dto.AppPushReceivers;
import cn.blmdz.wolf.msg.model.Channel;
import cn.blmdz.wolf.msg.model.Message;

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
