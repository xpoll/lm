package cn.blmdz.wolf.cart.service;

import cn.blmdz.home.common.model.Response;

public interface CartReadService {
   Response count(Long var1);

   Response listByUser(Long var1);
}
