package io.terminus.parana.web.msg.impl.common.simple;

import io.terminus.parana.web.msg.MsgReceiversTranslator;
import org.springframework.stereotype.Component;

@Component
public class SimpleMsgReceiversTranslator implements MsgReceiversTranslator {
   public String translateReceivers(String receivers) {
      return receivers;
   }
}
