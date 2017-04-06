package cn.blmdz.wolf.msg.component.validate;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.msg.component.MessageValidator;
import cn.blmdz.wolf.msg.model.Channel;
import cn.blmdz.wolf.msg.model.Message;

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
