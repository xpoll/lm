package cn.blmdz.wolf.web.msg.impl.notify.common;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.common.util.Json;
import cn.blmdz.wolf.msg.dto.NotificationDto;
import cn.blmdz.wolf.msg.model.Message;
import cn.blmdz.wolf.msg.model.Message.GroupMessageType;
import cn.blmdz.wolf.msg.service.NotifyWriteService;
import cn.blmdz.wolf.web.msg.MsgSender;

@Component
public class NotifySender implements MsgSender {
   private static final Logger log = LoggerFactory.getLogger(NotifySender.class);
   private final NotifyWriteService notifyWriteService;

   @Autowired
   public NotifySender(NotifyWriteService notifyWriteService) {
      this.notifyWriteService = notifyWriteService;
   }

   public Response send(Message message) {
      try {
         NotificationDto notificationDto = new NotificationDto();
         notificationDto.setSubject(message.getTitle());
         notificationDto.setContent(message.getContent());
         if(message.getGroupMessageType().equals(Integer.valueOf(GroupMessageType.NotGroupMessage.value()))) {
            if(message.getReceivers().contains("\"") && message.getReceivers().contains("[")) {
               List<String> receivers = (List)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(message.getReceivers(), Json.List_String);
               notificationDto.setAudienceIds(new ArrayList());

               for(String id : receivers) {
                  notificationDto.getAudienceIds().add(Long.valueOf(Long.parseLong(id)));
               }
            } else {
               List<Long> receivers = (List)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(message.getReceivers(), Json.List_Long);
               notificationDto.setAudienceIds(receivers);
            }
         } else {
            Map<String, String> groups = (Map)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(message.getReceivers(), Json.Map_String_String);
            notificationDto.setAudienceGroup1((String)groups.get("group1"));
            notificationDto.setAudienceGroup2((String)groups.get("group2"));
            notificationDto.setAudienceGroup3((String)groups.get("group3"));
            notificationDto.setAudienceGroup4((String)groups.get("group4"));
         }

         Response<String> response = this.notifyWriteService.sendNotification(notificationDto);
         return response;
      } catch (Exception var6) {
         return Response.fail(var6.getMessage());
      }
   }
}
