package io.terminus.parana.order.service;

import io.terminus.common.model.Response;
import java.util.List;

public interface OrderReadService {
   Response findMergeOrderById(Long var1);

   Response findMergeOrderRefundById(Long var1);

   Response findShopOrderById(Long var1);

   Response findSkuOrderById(Long var1);

   Response findShopOrderRefundById(Long var1);

   Response findSkuOrderRefundById(Long var1);

   Response findMergeOrderByIds(List var1);

   Response findShopOrderByIds(List var1);

   Response findSkuOrderByIds(List var1);

   Response findMergeOrderBy(Long var1, String var2, List var3, String var4, String var5, Integer var6, Integer var7);

   Response findShopOrderBy(Long var1, Long var2, String var3, List var4, String var5, String var6, Integer var7, Integer var8);

   Response findSkuOrderBy(Long var1, Long var2, String var3, List var4, String var5, String var6, Integer var7, Integer var8);

   Response findSkuOrderRefundBy(Long var1, Long var2, List var3, String var4, List var5, String var6, String var7, Integer var8, Integer var9);

   Response findShopOrderByParentId(Long var1);

   Response findSkuOrderByParentId(Long var1);

   Response findSkuOrderRefundByParentId(Long var1);

   Response findShopOrderRefundByParentId(Long var1);

   Response findShopOrderByParentIds(List var1);

   Response findSkuOrderByParentIds(List var1);

   Response findSkuOrderRefundByParentIds(List var1);
}
