package io.terminus.parana.storage.impl.manager;

import com.google.common.base.Objects;
import io.terminus.common.exception.ServiceException;
import io.terminus.parana.item.impl.dao.ItemDao;
import io.terminus.parana.item.impl.dao.SkuDao;
import io.terminus.parana.item.model.Item;
import io.terminus.parana.item.model.Sku;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.annotation.Transactional;

public class DefaultStorageManager {
   private static final Logger log = LoggerFactory.getLogger(DefaultStorageManager.class);
   private final ItemDao itemDao;
   private final SkuDao skuDao;

   public DefaultStorageManager(ItemDao itemDao, SkuDao skuDao) {
      this.itemDao = itemDao;
      this.skuDao = skuDao;
   }

   @Transactional
   public void decreaseBy(Long productId, Integer productType, Integer warehouseId, Integer delta) {
      Sku sku = (Sku)this.skuDao.findById(productId);
      if(sku == null) {
         log.error("sku(id={}) not found", productId);
         throw new ServiceException("sku.not.found");
      } else {
         Long skuId = sku.getId();
         Long itemId = sku.getItemId();
         Item item = (Item)this.itemDao.findById(itemId);
         this.checkStockIfEnough(item, sku, delta);
         this.skuDao.updateStockQuantity(skuId, delta);
         this.itemDao.updateSaleQuantity(itemId, delta);
         if(Objects.equal(item.getStatus(), Integer.valueOf(1)) && item.getStockQuantity().intValue() - delta.intValue() <= 0) {
            this.itemDao.updateStatus(itemId, Integer.valueOf(-1));
         }

         if(Objects.equal(sku.getStatus(), Integer.valueOf(1)) && sku.getStockQuantity().intValue() - delta.intValue() <= 0) {
            this.skuDao.updateStatusBySkuId(skuId, Integer.valueOf(-1));
         }

      }
   }

   private void checkStockIfEnough(Item item, Sku sku, Integer delta) {
      if(item.getStockQuantity().intValue() - delta.intValue() < 0) {
         log.error("stock quantity not enough where item id={},expect quantity:{},but actual quantity:{}", new Object[]{item.getId(), delta, item.getStockQuantity()});
         throw new ServiceException("item.stock.quantity.not.enough");
      } else if(sku.getStockQuantity().intValue() - delta.intValue() < 0) {
         log.error("stock quantity not enough where sku id={},expect quantity:{},but actual quantity:{}", new Object[]{sku.getId(), delta, sku.getStockQuantity()});
         throw new ServiceException("sku.stock.quantity.not.enough");
      }
   }
}
