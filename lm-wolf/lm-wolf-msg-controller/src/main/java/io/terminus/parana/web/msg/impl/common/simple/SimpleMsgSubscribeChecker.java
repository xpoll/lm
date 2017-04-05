package io.terminus.parana.web.msg.impl.common.simple;

import io.terminus.parana.msg.model.Message;
import io.terminus.parana.web.msg.MsgSubscribeChecker;
import org.springframework.stereotype.Component;

@Component
public class SimpleMsgSubscribeChecker implements MsgSubscribeChecker {
   public void checkSubscribe(Message message) {
   }
}
