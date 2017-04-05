package io.terminus.parana.order.service;

import io.terminus.common.model.Response;
import java.util.List;

public interface OrderNodeReadService {
   Response getEntranceByFlowId(Long var1);

   Response findById(Long var1);

   Response findByIds(List var1);
}
