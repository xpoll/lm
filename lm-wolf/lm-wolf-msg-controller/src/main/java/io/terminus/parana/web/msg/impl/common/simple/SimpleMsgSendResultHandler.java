package io.terminus.parana.web.msg.impl.common.simple;

import io.terminus.parana.msg.model.Message;
import io.terminus.parana.web.msg.MsgSendResultHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class SimpleMsgSendResultHandler implements MsgSendResultHandler {
   private static final Logger log = LoggerFactory.getLogger(SimpleMsgSendResultHandler.class);

   public void handlerResult(Message message) {
      log.info("message send result={}, channelOutput={},failReason={}", new Object[]{message.getStatus(), message.getChannelOutput(), message.getFailReason()});
   }
}
