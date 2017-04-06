package cn.blmdz.wolf.order.cache;

import cn.blmdz.home.common.model.Response;

public interface TradeFlowCache {
   Response getOrderActionInstance(Long var1);

   Response getOrderNodeInstance(Long var1);
}
	