package io.terminus.parana.shop.service;

import io.terminus.common.model.Response;
import java.util.Map;

public interface AdminShopWriteService {
   Response frozen(Long var1);

   Response unFrozen(Long var1);

   Response updateTags(Long var1, Map var2);
}
