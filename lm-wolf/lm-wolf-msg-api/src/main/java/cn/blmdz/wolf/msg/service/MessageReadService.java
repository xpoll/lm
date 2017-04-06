package cn.blmdz.wolf.msg.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.dto.MessageCriteria;

public interface MessageReadService {
   Response pagingMessages(MessageCriteria var1);

   Response findMessageById(Long var1);

   Response getMessageStatus(Long var1);

   Response getMessageActualReceivers(Long var1);

   Response getMessageActualReceivers(String var1);
}
