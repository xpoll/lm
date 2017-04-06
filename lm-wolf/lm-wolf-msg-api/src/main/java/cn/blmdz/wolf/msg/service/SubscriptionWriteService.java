package cn.blmdz.wolf.msg.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.model.Subscription;

public interface SubscriptionWriteService {
   Response createSubscription(Subscription var1);

   Response updateSubscription(Subscription var1);

   Response deleteSubscriptionById(Long var1);
}
