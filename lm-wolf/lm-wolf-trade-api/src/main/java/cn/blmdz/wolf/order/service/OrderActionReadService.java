package cn.blmdz.wolf.order.service;

import cn.blmdz.home.common.model.Response;

public interface OrderActionReadService {
   Response findActionInstancesByActionType(Long var1, Integer var2);

   Response findExcludedActionsGroupByUserType(Long var1, Long... var2);

   Response findById(Long var1);
}
