package cn.blmdz.wolf.msg.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.dto.SubscriptionCriteria;

public interface SubscriptionReadService {
   Response findSubscriptionById(Long var1);

   Response pagingSubscriptions(SubscriptionCriteria var1);

   Response findSubscriptionByAccount(String var1);

   Response findSubscriptionByUserId(Long var1);
}
