package io.terminus.parana.shop.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.shop.impl.dao.ShopDao;
import io.terminus.parana.shop.model.Shop;
import io.terminus.parana.shop.service.ShopWriteService;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ShopWriteServiceImpl implements ShopWriteService {
   private static final Logger log = LoggerFactory.getLogger(ShopWriteServiceImpl.class);
   private final ShopDao shopDao;

   @Autowired
   public ShopWriteServiceImpl(ShopDao shopDao) {
      this.shopDao = shopDao;
   }

   public Response create(Shop shop) {
      try {
         this.shopDao.create(shop);
         return Response.ok(shop.getId());
      } catch (Exception var3) {
         log.error("failed to create {}, cause:{}", shop, Throwables.getStackTraceAsString(var3));
         return Response.fail("shop.create.fail");
      }
   }

   public Response update(Shop shop) {
      try {
         shop.setStatus((Integer)null);
         shop.setTags((Map)null);
         this.shopDao.update(shop);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to update {}, cause:{}", shop, Throwables.getStackTraceAsString(var3));
         return Response.fail("shop.update.fail");
      }
   }
}
