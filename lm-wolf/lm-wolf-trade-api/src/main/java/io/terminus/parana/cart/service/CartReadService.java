package io.terminus.parana.cart.service;

import io.terminus.common.model.Response;

public interface CartReadService {
   Response count(Long var1);

   Response listByUser(Long var1);
}
