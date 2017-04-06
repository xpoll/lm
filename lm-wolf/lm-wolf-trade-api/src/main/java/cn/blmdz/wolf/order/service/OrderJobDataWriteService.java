package cn.blmdz.wolf.order.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.order.model.OrderJobData;

public interface OrderJobDataWriteService {
   Response create(OrderJobData var1);

   Response update(OrderJobData var1);

   Response updateBy(Long var1, Integer var2, Long var3, Integer var4);
}
