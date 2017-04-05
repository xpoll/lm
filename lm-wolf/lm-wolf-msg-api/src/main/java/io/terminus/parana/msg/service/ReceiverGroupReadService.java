package io.terminus.parana.msg.service;

import io.terminus.common.model.Response;
import io.terminus.parana.msg.dto.ReceiverGroupCriteria;

public interface ReceiverGroupReadService {
   Response findReceiverGroupById(Long var1);

   Response findReceiverGroupByUserId(Long var1);

   Response pagingReceiverGroups(ReceiverGroupCriteria var1);
}
