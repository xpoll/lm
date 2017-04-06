package cn.blmdz.wolf.parana.item.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.parana.item.dto.FullItem;

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
