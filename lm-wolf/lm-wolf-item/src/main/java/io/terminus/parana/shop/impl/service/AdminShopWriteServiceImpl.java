package io.terminus.parana.shop.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.shop.impl.dao.ShopDao;
import io.terminus.parana.shop.service.AdminShopWriteService;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class AdminShopWriteServiceImpl implements AdminShopWriteService {
   private static final Logger log = LoggerFactory.getLogger(AdminShopWriteServiceImpl.class);
   private final ShopDao shopDao;

   @Autowired
   public AdminShopWriteServiceImpl(ShopDao shopDao) {
      this.shopDao = shopDao;
   }

   public Response frozen(Long shopId) {
      try {
         this.shopDao.updateStatus(shopId, Integer.valueOf(-2));
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to frozen shop(id={}), cause:{}", shopId, Throwables.getStackTraceAsString(var3));
         return Response.fail("shop.update.fail");
      }
   }

   public Response unFrozen(Long shopId) {
      try {
         this.shopDao.updateStatus(shopId, Integer.valueOf(1));
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to unfrozen shop(id={}), cause:{}", shopId, Throwables.getStackTraceAsString(var3));
         return Response.fail("shop.update.fail");
      }
   }

   public Response updateTags(Long shopId, Map tags) {
      try {
         this.shopDao.updateTags(shopId, tags);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("failed to update shop(id={}) tags to {}, cause:{}", shopId, tags);
         return Response.fail("shop.update.fail");
      }
   }
}
