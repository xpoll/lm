package cn.blmdz.wolf.parana.item.service;

import java.util.List;
import java.util.Map;

import cn.blmdz.home.common.model.Response;

public interface AdminItemWriteService {
   Response batchUpdateStatusByShopId(Long var1, Integer var2);

   Response batchUpdateStatus(List var1, Integer var2);

   Response updateStatus(Long var1, Integer var2);

   Response tags(Long var1, Map var2);
}
