package io.terminus.parana.msg.service;

import io.terminus.common.model.Response;
import io.terminus.parana.msg.dto.SubscriptionCriteria;

public interface SubscriptionReadService {
   Response findSubscriptionById(Long var1);

   Response pagingSubscriptions(SubscriptionCriteria var1);

   Response findSubscriptionByAccount(String var1);

   Response findSubscriptionByUserId(Long var1);
}
