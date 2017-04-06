package cn.blmdz.wolf.cart.service;

import java.util.List;
import java.util.Map;

import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.parana.item.model.Sku;

public interface CartWriteService {
   Response changeCart(Sku var1, Integer var2, Long var3);

   Response changeCart(Map<Sku, Integer> skuAndQuantities, Long userId);

   Response deleteById(Long var1);

   Response deleteCart(Long var1);

   Response batchDelete(List var1, Long var2);

   Response submitCart(Map var1, BaseUser var2);
}
