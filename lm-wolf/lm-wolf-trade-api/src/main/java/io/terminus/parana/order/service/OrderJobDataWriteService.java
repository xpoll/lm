package io.terminus.parana.order.service;

import io.terminus.common.model.Response;
import io.terminus.parana.order.model.OrderJobData;

public interface OrderJobDataWriteService {
   Response create(OrderJobData var1);

   Response update(OrderJobData var1);

   Response updateBy(Long var1, Integer var2, Long var3, Integer var4);
}
