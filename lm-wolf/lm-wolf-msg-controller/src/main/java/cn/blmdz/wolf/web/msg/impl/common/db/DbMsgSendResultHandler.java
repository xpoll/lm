package cn.blmdz.wolf.web.msg.impl.common.db;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.model.Message;
import cn.blmdz.wolf.msg.service.MessageWriteService;
import cn.blmdz.wolf.web.msg.MsgSendResultHandler;

@Component
public class DbMsgSendResultHandler implements MsgSendResultHandler {
   private static final Logger log = LoggerFactory.getLogger(DbMsgSendResultHandler.class);
   protected final MessageWriteService messageWriteService;

   @Autowired
   public DbMsgSendResultHandler(MessageWriteService messageWriteService) {
      this.messageWriteService = messageWriteService;
   }

   public void handlerResult(Message message) {
      Message toUpdate = new Message();
      toUpdate.setId(message.getId());
      toUpdate.setStatus(message.getStatus());
      toUpdate.setChannelOutput(message.getChannelOutput());
      toUpdate.setFailReason(message.getFailReason());
      Response<Boolean> response = this.messageWriteService.updateMessage(message);
      if(!response.isSuccess()) {
         log.error("updateMessage failed, message={}, cause={}", toUpdate, response.getError());
      }

   }
}
