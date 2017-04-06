package cn.blmdz.wolf.order.service;

import java.util.List;
import java.util.Map;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.order.dto.OrderChain;
import cn.blmdz.wolf.order.model.Order;

public interface OrderWriteService {
   Response create(OrderChain var1);

   Response create(Order var1, Map var2);

   Response update(List var1, Map var2);

   Response update(Order var1, Map var2);
}
