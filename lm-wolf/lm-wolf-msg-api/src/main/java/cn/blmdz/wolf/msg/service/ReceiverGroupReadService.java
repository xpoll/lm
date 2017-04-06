package cn.blmdz.wolf.msg.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.dto.ReceiverGroupCriteria;

public interface ReceiverGroupReadService {
   Response findReceiverGroupById(Long var1);

   Response findReceiverGroupByUserId(Long var1);

   Response pagingReceiverGroups(ReceiverGroupCriteria var1);
}
