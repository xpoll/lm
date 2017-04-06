package cn.blmdz.wolf.order.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;

public interface OrderNodeReadService {
   Response getEntranceByFlowId(Long var1);

   Response findById(Long var1);

   Response findByIds(List var1);
}
