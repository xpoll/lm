package io.terminus.parana.item.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.item.impl.dao.SkuDao;
import io.terminus.parana.item.model.Sku;
import io.terminus.parana.item.service.SkuReadService;
import java.util.Collections;
import java.util.List;
import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SkuReadServiceImpl implements SkuReadService {
   private static final Logger log = LoggerFactory.getLogger(SkuReadServiceImpl.class);
   private final SkuDao skuDao;

   @Autowired
   public SkuReadServiceImpl(SkuDao skuDao) {
      this.skuDao = skuDao;
   }

   public Response findSkuById(Long id) {
      try {
         Sku sku = (Sku)this.skuDao.findById(id);
         if(sku == null) {
            log.error("sku(id={}) not found", id);
            return Response.fail("sku.not.found");
         } else {
            return Response.ok(sku);
         }
      } catch (Exception var3) {
         log.error("failed to find sku(id={}), cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("sku.find.fail");
      }
   }

   public Response findSkusByIds(List skuIds) {
      if(CollectionUtils.isEmpty(skuIds)) {
         return Response.ok(Collections.emptyList());
      } else {
         try {
            List<Sku> skus = this.skuDao.findByIds(skuIds);
            return Response.ok(skus);
         } catch (Exception var3) {
            log.error("failed to find skus(ids={}), cause:{}", skuIds, Throwables.getStackTraceAsString(var3));
            return Response.fail("sku.find.fail");
         }
      }
   }

   public Response findSkuByCode(Long shopId, String skuCode) {
      try {
         List<Sku> skus = this.skuDao.findByShopIdAndSkuCode(shopId, skuCode);
         return Response.ok(skus);
      } catch (Exception var4) {
         log.error("failed to find skus by (shopId={}, skuCode={}), cause:{}", new Object[]{shopId, skuCode, Throwables.getStackTraceAsString(var4)});
         return Response.fail("sku.find.fail");
      }
   }

   public Response findSkusByItemId(Long itemId) {
      try {
         List<Sku> skus = this.skuDao.findByItemId(itemId);
         return Response.ok(skus);
      } catch (Exception var3) {
         log.error("failed to find skus by (itemId={}), cause:{}", itemId, Throwables.getStackTraceAsString(var3));
         return Response.fail("sku.find.fail");
      }
   }
}
