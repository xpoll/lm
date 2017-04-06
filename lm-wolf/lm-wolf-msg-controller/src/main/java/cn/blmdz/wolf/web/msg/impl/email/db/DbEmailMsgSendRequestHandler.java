package cn.blmdz.wolf.web.msg.impl.email.db;

import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.wolf.msg.model.Channel;
import cn.blmdz.wolf.msg.model.Message;
import cn.blmdz.wolf.msg.service.MessageReadService;
import cn.blmdz.wolf.msg.service.MessageWriteService;
import cn.blmdz.wolf.web.msg.MsgSendRequestHandler;
import cn.blmdz.wolf.web.msg.impl.common.db.DbMsgSendRequestHandlerBase;

@Component
public class DbEmailMsgSendRequestHandler extends DbMsgSendRequestHandlerBase implements MsgSendRequestHandler {
   private static final Logger log = LoggerFactory.getLogger(DbEmailMsgSendRequestHandler.class);

   @Autowired
   public DbEmailMsgSendRequestHandler(MessageReadService messageReadService, MessageWriteService messageWriteService) {
      super(messageReadService, messageWriteService);
   }

   public Message request(String toes, String templateName, Map context, String extra) {
      return super.request(Integer.valueOf(Channel.Email.value()), toes, templateName, context, extra);
   }

   public Message request(String toes, String title, String content, String extra) {
      return super.request(Integer.valueOf(Channel.Email.value()), toes, title, content, extra);
   }

   public Message request(Message message) {
      message.setChannel(Integer.valueOf(Channel.Email.value()));
      return super.request(message);
   }
}
