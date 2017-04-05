package io.terminus.parana.web.msg.impl.apppush.common;

import io.terminus.common.model.Response;
import io.terminus.common.utils.JsonMapper;
import io.terminus.lib.apppush.AppPushService;
import io.terminus.parana.msg.dto.AppPushReceivers;
import io.terminus.parana.msg.model.Message;
import io.terminus.parana.web.msg.MsgGatewayBuilder;
import io.terminus.parana.web.msg.MsgSender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class AppPushSender implements MsgSender {
   private static final Logger log = LoggerFactory.getLogger(AppPushSender.class);
   private final MsgGatewayBuilder msgGatewayBuilder;

   @Autowired
   public AppPushSender(MsgGatewayBuilder msgGatewayBuilder) {
      this.msgGatewayBuilder = msgGatewayBuilder;
   }

   public Response send(Message message) {
      try {
         AppPushReceivers receivers = (AppPushReceivers)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(message.getReceivers(), AppPushReceivers.class);
         if(receivers.getAndroid().size() > 0) {
            AppPushService appPushService = this.msgGatewayBuilder.buildAppPushService("android");
            String result = appPushService.send("android", JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(receivers.getAndroid()), message.getTitle(), message.getContent());
            return Response.ok(result);
         } else if(receivers.getIos().size() > 0) {
            AppPushService appPushService = this.msgGatewayBuilder.buildAppPushService("ios");
            String result = appPushService.send("ios", JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(receivers.getIos()), message.getTitle(), message.getContent());
            return Response.ok(result);
         } else {
            return Response.ok();
         }
      } catch (Exception var5) {
         return Response.fail(var5.getMessage());
      }
   }
}
