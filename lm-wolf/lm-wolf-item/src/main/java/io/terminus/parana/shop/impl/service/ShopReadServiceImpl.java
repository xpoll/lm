package io.terminus.parana.shop.impl.service;

import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import io.terminus.common.model.Response;
import io.terminus.parana.shop.impl.dao.ShopDao;
import io.terminus.parana.shop.model.Shop;
import io.terminus.parana.shop.service.ShopReadService;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

@Service
public class ShopReadServiceImpl implements ShopReadService {
   private static final Logger log = LoggerFactory.getLogger(ShopReadServiceImpl.class);
   private final ShopDao shopDao;

   @Autowired
   public ShopReadServiceImpl(ShopDao shopDao) {
      this.shopDao = shopDao;
   }

   public Response findById(Long shopId) {
      try {
         Shop shop = (Shop)this.shopDao.findById(shopId);
         if(shop == null) {
            log.error("no shop(id={}) found", shopId);
            return Response.fail("shop.not.found");
         } else {
            return Response.ok(shop);
         }
      } catch (Exception var3) {
         log.error("failed to find shop by id={}, cause:{}", shopId, Throwables.getStackTraceAsString(var3));
         return Response.fail("shop.find.fail");
      }
   }

   public Response findByIds(List shopIds) {
      try {
         if(shopIds != null && !shopIds.isEmpty()) {
            return Response.ok(this.shopDao.findByIds(shopIds));
         } else {
            List<Shop> emptyShops = Lists.newArrayList();
            return Response.ok(emptyShops);
         }
      } catch (Exception var3) {
         log.error("find shops by ids={} failed, cause:{}", shopIds, Throwables.getStackTraceAsString(var3));
         return Response.fail("shop.find.fail");
      }
   }

   public Response findByOuterId(String outerId) {
      try {
         Shop shop = this.shopDao.findByOuterId(outerId);
         if(shop == null) {
            log.error("no shop(outerId={}) found", outerId);
            return Response.fail("shop.not.found");
         } else {
            return Response.ok(shop);
         }
      } catch (Exception var3) {
         log.error("failed to find shop by outerId={}, cause:{}", outerId, Throwables.getStackTraceAsString(var3));
         return Response.fail("shop.find.fail");
      }
   }

   public Response findByUserId(Long userId) {
      try {
         Shop shop = this.shopDao.findByUserId(userId);
         if(shop == null) {
            log.error("no shop(userId={}) found", userId);
            return Response.fail("shop.not.found");
         } else {
            return Response.ok(shop);
         }
      } catch (Exception var3) {
         log.error("failed to find shop by userId={}, cause:{}", userId, Throwables.getStackTraceAsString(var3));
         return Response.fail("shop.find.fail");
      }
   }

   public Response findByName(String name) {
      if(!StringUtils.hasText(name)) {
         log.error("no shop name specified");
         return Response.fail("shop.name.empty");
      } else {
         try {
            Shop shop = this.shopDao.findByName(name.trim().toLowerCase());
            if(shop == null) {
               log.error("no shop(name={}) found", name);
               return Response.fail("shop.not.found");
            } else {
               return Response.ok(shop);
            }
         } catch (Exception var3) {
            log.error("failed to find shop by name={}, cause:{}", name, Throwables.getStackTraceAsString(var3));
            return Response.fail("shop.find.fail");
         }
      }
   }
}
