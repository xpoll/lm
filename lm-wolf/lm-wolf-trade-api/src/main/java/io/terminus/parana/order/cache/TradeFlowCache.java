package io.terminus.parana.order.cache;

import io.terminus.common.model.Response;

public interface TradeFlowCache {
   Response getOrderActionInstance(Long var1);

   Response getOrderNodeInstance(Long var1);
}
