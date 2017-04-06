package cn.blmdz.wolf.order.service;

import cn.blmdz.home.common.model.Response;

public interface OrderJobDataReadService {
   Response listTo(Long var1, Integer var2);

   Response lastId();
}
