package cn.blmdz.wolf.order.service;

import java.util.List;
import java.util.Map;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.order.model.OrderActionInstance;

public interface OrderActionReadService {
   Response findActionInstancesByActionType(Long var1, Integer var2);

   Response<Map<Integer, List<OrderActionInstance>>> findExcludedActionsGroupByUserType(Long var1, Long... var2);

   Response findById(Long var1);
}
