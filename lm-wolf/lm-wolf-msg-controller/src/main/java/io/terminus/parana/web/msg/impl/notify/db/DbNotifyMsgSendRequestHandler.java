package io.terminus.parana.web.msg.impl.notify.db;

import io.terminus.parana.msg.model.Channel;
import io.terminus.parana.msg.model.Message;
import io.terminus.parana.msg.service.MessageReadService;
import io.terminus.parana.msg.service.MessageWriteService;
import io.terminus.parana.web.msg.MsgSendRequestHandler;
import io.terminus.parana.web.msg.impl.common.db.DbMsgSendRequestHandlerBase;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DbNotifyMsgSendRequestHandler extends DbMsgSendRequestHandlerBase implements MsgSendRequestHandler {
   private static final Logger log = LoggerFactory.getLogger(DbNotifyMsgSendRequestHandler.class);

   @Autowired
   public DbNotifyMsgSendRequestHandler(MessageReadService messageReadService, MessageWriteService messageWriteService) {
      super(messageReadService, messageWriteService);
   }

   public Message request(String toes, String templateName, Map context, String extra) {
      return super.request(Integer.valueOf(Channel.Notification.value()), toes, templateName, context, extra);
   }

   public Message request(String toes, String title, String content, String extra) {
      return super.request(Integer.valueOf(Channel.Notification.value()), toes, title, content, extra);
   }

   public Message request(Message message) {
      message.setChannel(Integer.valueOf(Channel.Notification.value()));
      return super.request(message);
   }
}
