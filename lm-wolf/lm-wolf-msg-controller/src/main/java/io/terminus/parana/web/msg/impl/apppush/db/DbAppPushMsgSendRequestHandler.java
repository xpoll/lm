package io.terminus.parana.web.msg.impl.apppush.db;

import io.terminus.parana.msg.model.Channel;
import io.terminus.parana.msg.model.Message;
import io.terminus.parana.msg.service.MessageReadService;
import io.terminus.parana.msg.service.MessageWriteService;
import io.terminus.parana.web.msg.MsgSendRequestHandler;
import io.terminus.parana.web.msg.impl.common.db.DbMsgSendRequestHandlerBase;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DbAppPushMsgSendRequestHandler extends DbMsgSendRequestHandlerBase implements MsgSendRequestHandler {
   @Autowired
   public DbAppPushMsgSendRequestHandler(MessageReadService messageReadService, MessageWriteService messageWriteService) {
      super(messageReadService, messageWriteService);
   }

   public Message request(String toes, String templateName, Map context, String extra) {
      return super.request(Integer.valueOf(Channel.AppPush.value()), toes, templateName, context, extra);
   }

   public Message request(String toes, String title, String content, String extra) {
      return super.request(Integer.valueOf(Channel.AppPush.value()), toes, title, content, extra);
   }

   public Message request(Message message) {
      message.setChannel(Integer.valueOf(Channel.AppPush.value()));
      return super.request(message);
   }
}
