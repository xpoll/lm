package cn.blmdz.wolf.item.impl.manager;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Objects;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.wolf.category.impl.dao.ShopCategoryItemDao;
import cn.blmdz.wolf.category.model.ShopCategoryItem;
import cn.blmdz.wolf.item.impl.dao.ItemAttributeDao;
import cn.blmdz.wolf.item.impl.dao.ItemDao;
import cn.blmdz.wolf.item.impl.dao.ItemDetailDao;
import cn.blmdz.wolf.item.impl.dao.SkuDao;
import cn.blmdz.wolf.item.model.Item;
import cn.blmdz.wolf.item.model.ItemAttribute;
import cn.blmdz.wolf.item.model.ItemDetail;
import cn.blmdz.wolf.item.model.Sku;

@Component
public class ItemManager {
   private static final Logger log = LoggerFactory.getLogger(ItemManager.class);
   private final ItemDao itemDao;
   private final SkuDao skuDao;
   private final ItemDetailDao itemDetailDao;
   private final ItemAttributeDao itemAttributeDao;
   private final ShopCategoryItemDao shopCategoryItemDao;

   @Autowired
   public ItemManager(ItemDao itemDao, SkuDao skuDao, ItemDetailDao itemDetailDao, ItemAttributeDao itemAttributeDao, ShopCategoryItemDao shopCategoryItemDao) {
      this.itemDao = itemDao;
      this.skuDao = skuDao;
      this.itemDetailDao = itemDetailDao;
      this.itemAttributeDao = itemAttributeDao;
      this.shopCategoryItemDao = shopCategoryItemDao;
   }

   @Transactional
   public Long createItem(Item item, ItemDetail itemDetail, ItemAttribute itemAttribute, List skus) {
      this.itemDao.create(item);
      Long itemId = item.getId();
      itemDetail.setItemId(itemId);
      this.itemDetailDao.create(itemDetail);
      itemAttribute.setItemId(itemId);
      this.itemAttributeDao.create(itemAttribute);
      this.initShopCategory(item);
      this.persistSkus(item, skus);
      return itemId;
   }

   private void persistSkus(Item item, List<Sku> skus) {
      for(Sku sku : skus) {
         sku.setItemId(item.getId());
         sku.setShopId(item.getShopId());
         sku.setStatus(item.getStatus());
         sku.setStockType(item.getStockType());
         this.skuDao.create(sku);
      }

   }

   private void initShopCategory(Item item) {
      ShopCategoryItem rootCategory = new ShopCategoryItem();
      rootCategory.setShopId(item.getShopId());
      rootCategory.setItemId(item.getId());
      rootCategory.setShopCategoryId(Long.valueOf(0L));
      this.shopCategoryItemDao.create(rootCategory);
   }

   @Transactional
   public void updateItem(Item item, ItemDetail itemDetail, ItemAttribute itemAttribute, List skus) {
      this.itemDao.update(item);
      Long itemId = item.getId();
      itemDetail.setItemId(itemId);
      this.itemDetailDao.update(itemDetail);
      itemAttribute.setItemId(itemId);
      this.itemAttributeDao.update(itemAttribute);
      this.skuDao.updateStatusByItemId(itemId, Integer.valueOf(-3));
      this.createOrUpdateSkus(item, skus);
   }

   private void createOrUpdateSkus(Item item, List<Sku> skus) {
      for(Sku sku : skus) {
         sku.setItemId(item.getId());
         sku.setShopId(item.getShopId());
         sku.setStatus(item.getStatus());
         sku.setStockType(item.getStockType());
         if(sku.getId() != null) {
            this.skuDao.update(sku);
         } else {
            this.skuDao.create(sku);
         }
      }

   }

   @Transactional
   public Item delete(Long shopId, Long itemId) {
      Item item = (Item)this.itemDao.findById(itemId);
      if(item == null) {
         log.warn("item(id={}) isn\'t exist.", itemId);
         throw new ServiceException("item.not.exist");
      } else if(!Objects.equal(shopId, item.getShopId())) {
         log.warn("shop(id={}) isn\'t owner of item({})", shopId, item);
         throw new ServiceException("user.not.owner");
      } else {
         this.itemDao.updateStatus(itemId, Integer.valueOf(-3));
         this.skuDao.updateStatusByItemId(itemId, Integer.valueOf(-3));
         this.shopCategoryItemDao.deleteByShopIdAndItemId(shopId, itemId);
         return item;
      }
   }

   @Transactional
   public void batchUpdateStatusByShopIdAndItemIds(Long shopId, List itemIds, Integer status) {
      boolean success = this.itemDao.batchUpdateStatusByShopIdAndIds(shopId, itemIds, status);
      if(!success) {
         log.error("failed to update items(ids={}) to status({})", itemIds, status);
         throw new ServiceException("item.status.update.fail");
      } else {
         this.skuDao.updateStatusByItemIds(itemIds, status);
      }
   }

   @Transactional
   public void updateStatusByShopIdAndItemId(Long shopId, Long itemId, Integer status) {
      boolean success = this.itemDao.updateStatusByShopIdAndItemId(shopId, itemId, status);
      if(!success) {
         log.error("failed to update item(id={}) to status({})", itemId, status);
         throw new ServiceException("item.status.update.fail");
      } else {
         this.skuDao.updateStatusByItemId(itemId, status);
      }
   }

   @Transactional
   public void updateStatusByItemId(Long itemId, Integer status) {
      this.itemDao.updateStatus(itemId, status);
      this.skuDao.updateStatusByItemId(itemId, status);
   }

   @Transactional
   public void updateRichText(String itemInfoMd5, ItemDetail itemDetail) {
      this.itemDao.updateItemInfoMd5(itemDetail.getItemId(), itemInfoMd5);
      this.itemDetailDao.update(itemDetail);
   }
}
