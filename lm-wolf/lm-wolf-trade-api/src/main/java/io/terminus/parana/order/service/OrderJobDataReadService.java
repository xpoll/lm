package io.terminus.parana.order.service;

import io.terminus.common.model.Response;

public interface OrderJobDataReadService {
   Response listTo(Long var1, Integer var2);

   Response lastId();
}
