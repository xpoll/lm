package io.terminus.parana.msg.service;

import io.terminus.common.model.Response;
import io.terminus.parana.msg.model.Subscription;

public interface SubscriptionWriteService {
   Response createSubscription(Subscription var1);

   Response updateSubscription(Subscription var1);

   Response deleteSubscriptionById(Long var1);
}
