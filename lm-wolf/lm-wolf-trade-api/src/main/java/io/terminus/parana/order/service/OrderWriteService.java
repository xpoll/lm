package io.terminus.parana.order.service;

import io.terminus.common.model.Response;
import io.terminus.parana.order.dto.OrderChain;
import io.terminus.parana.order.model.Order;
import java.util.List;
import java.util.Map;

public interface OrderWriteService {
   Response create(OrderChain var1);

   Response create(Order var1, Map var2);

   Response update(List var1, Map var2);

   Response update(Order var1, Map var2);
}
