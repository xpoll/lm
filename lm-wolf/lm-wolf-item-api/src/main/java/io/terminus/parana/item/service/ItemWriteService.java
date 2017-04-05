package io.terminus.parana.item.service;

import io.terminus.common.model.Response;
import io.terminus.parana.item.dto.FullItem;
import java.util.List;

public interface ItemWriteService {
   Response create(FullItem var1);

   Response update(FullItem var1);

   Response delete(Long var1, Long var2);

   Response updateStatusByShopIdAndItemId(Long var1, Long var2, Integer var3);

   Response updateStatusByItemId(Long var1, Integer var2);

   Response batchUpdateStatusByShopIdAndItemIds(Long var1, List var2, Integer var3);

   Response editRichText(Long var1, String var2);

   Response updateDigest(Long var1, String var2);
}
