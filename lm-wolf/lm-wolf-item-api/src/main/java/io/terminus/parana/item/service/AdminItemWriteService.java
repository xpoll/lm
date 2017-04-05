package io.terminus.parana.item.service;

import io.terminus.common.model.Response;
import java.util.List;
import java.util.Map;

public interface AdminItemWriteService {
   Response batchUpdateStatusByShopId(Long var1, Integer var2);

   Response batchUpdateStatus(List var1, Integer var2);

   Response updateStatus(Long var1, Integer var2);

   Response tags(Long var1, Map var2);
}
