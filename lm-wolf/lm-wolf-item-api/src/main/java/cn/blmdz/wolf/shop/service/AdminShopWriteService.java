package cn.blmdz.wolf.shop.service;

import java.util.Map;

import cn.blmdz.home.common.model.Response;

public interface AdminShopWriteService {
   Response frozen(Long var1);

   Response unFrozen(Long var1);

   Response updateTags(Long var1, Map var2);
}
