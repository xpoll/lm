package io.terminus.parana.msg.service;

import io.terminus.common.model.Response;
import io.terminus.parana.msg.dto.MessageCriteria;

public interface MessageReadService {
   Response pagingMessages(MessageCriteria var1);

   Response findMessageById(Long var1);

   Response getMessageStatus(Long var1);

   Response getMessageActualReceivers(Long var1);

   Response getMessageActualReceivers(String var1);
}
