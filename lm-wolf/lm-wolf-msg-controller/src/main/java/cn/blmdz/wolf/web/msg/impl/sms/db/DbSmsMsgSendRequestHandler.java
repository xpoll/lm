package cn.blmdz.wolf.web.msg.impl.sms.db;

import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.wolf.msg.model.Channel;
import cn.blmdz.wolf.msg.model.Message;
import cn.blmdz.wolf.msg.service.MessageReadService;
import cn.blmdz.wolf.msg.service.MessageWriteService;
import cn.blmdz.wolf.web.msg.MsgSendRequestHandler;
import cn.blmdz.wolf.web.msg.impl.common.db.DbMsgSendRequestHandlerBase;

@Component
public class DbSmsMsgSendRequestHandler extends DbMsgSendRequestHandlerBase implements MsgSendRequestHandler {
   @Autowired
   public DbSmsMsgSendRequestHandler(MessageReadService messageReadService, MessageWriteService messageWriteService) {
      super(messageReadService, messageWriteService);
   }

   public Message request(String toes, String templateName, Map context, String extra) {
      return super.request(Integer.valueOf(Channel.Sms.value()), toes, templateName, context, extra);
   }

   public Message request(String toes, String title, String content, String extra) {
      return super.request(Integer.valueOf(Channel.Sms.value()), toes, title, content, extra);
   }

   public Message request(Message message) {
      message.setChannel(Integer.valueOf(Channel.Sms.value()));
      return super.request(message);
   }
}
