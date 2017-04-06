package cn.blmdz.wolf.web.msg.impl.common.simple;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import cn.blmdz.wolf.msg.model.Message;
import cn.blmdz.wolf.web.msg.MsgSendResultHandler;

@Component
public class SimpleMsgSendResultHandler implements MsgSendResultHandler {
   private static final Logger log = LoggerFactory.getLogger(SimpleMsgSendResultHandler.class);

   public void handlerResult(Message message) {
      log.info("message send result={}, channelOutput={},failReason={}", new Object[]{message.getStatus(), message.getChannelOutput(), message.getFailReason()});
   }
}
