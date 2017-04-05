package io.terminus.parana.cart.service;

import io.terminus.common.model.BaseUser;
import io.terminus.common.model.Response;
import io.terminus.parana.item.model.Sku;
import java.util.List;
import java.util.Map;

public interface CartWriteService {
   Response changeCart(Sku var1, Integer var2, Long var3);

   Response changeCart(Map var1, Long var2);

   Response deleteById(Long var1);

   Response deleteCart(Long var1);

   Response batchDelete(List var1, Long var2);

   Response submitCart(Map var1, BaseUser var2);
}
