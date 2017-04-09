package cn.blmdz.wolf.storage.impl.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.storage.impl.manager.DefaultStorageManager;
import cn.blmdz.wolf.storage.service.StorageService;

public class DefaultStorageServiceImpl implements StorageService {
   private static final Logger log = LoggerFactory.getLogger(DefaultStorageServiceImpl.class);
   private final DefaultStorageManager defaultStorageManager;

   public DefaultStorageServiceImpl(DefaultStorageManager defaultStorageManager) {
      this.defaultStorageManager = defaultStorageManager;
   }

   public Response findBy(Long productId, Integer productType, Integer warehouseId) {
      throw new UnsupportedOperationException();
   }

   public Response decreaseBy(Long productId, Integer productType, Integer warehouseId, Integer delta) {
      try {
         this.defaultStorageManager.decreaseBy(productId, productType, warehouseId, delta);
         return Response.ok(Boolean.TRUE);
      } catch (ServiceException var6) {
         log.error("fail to decrease by productId:{},productType:{},warehouseId:{},delta:{},cause:{}", new Object[]{productId, productType, warehouseId, delta, Throwables.getStackTraceAsString(var6)});
         return Response.fail(var6.getMessage());
      } catch (Exception var7) {
         log.error("fail to decrease by productId:{},productType:{},warehouseId:{},delta:{},cause:{}", new Object[]{productId, productType, warehouseId, delta, Throwables.getStackTraceAsString(var7)});
         return Response.fail("decrease.storage.fail");
      }
   }

   public Response increaseBy(Long productId, Integer productType, Integer warehouseId, Integer delta) {
      throw new UnsupportedOperationException();
   }

   public Response set(Long productId, Integer productType, Integer warehouseId, Integer quantity) {
      throw new UnsupportedOperationException();
   }
}
