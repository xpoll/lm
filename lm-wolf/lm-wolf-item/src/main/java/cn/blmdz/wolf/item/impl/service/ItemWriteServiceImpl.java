package cn.blmdz.wolf.item.impl.service;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.category.impl.dao.BackCategoryDao;
import cn.blmdz.wolf.item.impl.dao.ItemAttributeDao;
import cn.blmdz.wolf.item.impl.dao.ItemDao;
import cn.blmdz.wolf.item.impl.dao.ItemDetailDao;
import cn.blmdz.wolf.item.impl.manager.ItemManager;
import cn.blmdz.wolf.parana.category.model.BackCategory;
import cn.blmdz.wolf.parana.item.common.Digestors;
import cn.blmdz.wolf.parana.item.dto.FullItem;
import cn.blmdz.wolf.parana.item.model.Item;
import cn.blmdz.wolf.parana.item.model.ItemAttribute;
import cn.blmdz.wolf.parana.item.model.ItemDetail;
import cn.blmdz.wolf.parana.item.service.ItemWriteService;

@Service
public class ItemWriteServiceImpl implements ItemWriteService {
   private static final Logger log = LoggerFactory.getLogger(ItemWriteServiceImpl.class);
   private final ItemManager itemManager;
   private final ItemDetailDao itemDetailDao;
   private final ItemDao itemDao;
   private final ItemAttributeDao itemAttributeDao;
   private final BackCategoryDao backCategoryDao;

   @Autowired
   public ItemWriteServiceImpl(ItemManager itemManager, ItemDetailDao itemDetailDao, ItemDao itemDao, ItemAttributeDao itemAttributeDao, BackCategoryDao backCategoryDao) {
      this.itemManager = itemManager;
      this.itemDetailDao = itemDetailDao;
      this.itemDao = itemDao;
      this.itemAttributeDao = itemAttributeDao;
      this.backCategoryDao = backCategoryDao;
   }

   public Response create(FullItem fullItem) {
      try {
         Item item = fullItem.getItem();
         Long categoryId = item.getCategoryId();
         BackCategory backCategory = (BackCategory)this.backCategoryDao.findById(categoryId);
         if(backCategory == null) {
            log.error("back category(id={}) not found", categoryId);
            return Response.fail("category.not.found");
         } else if(backCategory.getHasChildren().booleanValue()) {
            log.error("back category(id={}) is not leaf", categoryId);
            return Response.fail("category.not.leaf");
         } else {
            ItemAttribute itemAttribute = new ItemAttribute();
            itemAttribute.setOtherAttrs(fullItem.getGroupedOtherAttributes());
            itemAttribute.setSkuAttrs(fullItem.getGroupedSkuAttributes());
            String itemInfoMd5 = Digestors.itemDigest(item, fullItem.getItemDetail(), itemAttribute);
            item.setItemInfoMd5(itemInfoMd5);
            Long itemId = this.itemManager.createItem(item, fullItem.getItemDetail(), itemAttribute, fullItem.getSkus());
            return Response.ok(itemId);
         }
      } catch (Exception var8) {
         log.error("failed to create {}, cause:{}", fullItem, Throwables.getStackTraceAsString(var8));
         return Response.fail("item.create.fail");
      }
   }

   public Response update(FullItem fullItem) {
      try {
         Item item = fullItem.getItem();
         if(item.getStatus() == null) {
            Item itemInDB = (Item)this.itemDao.findById(item.getId());
            if(itemInDB == null) {
               log.error("item(id={}) not found", item.getId());
               return Response.fail("item.not.found");
            }

            item.setStatus(itemInDB.getStatus());
         }

         ItemAttribute itemAttribute = new ItemAttribute();
         itemAttribute.setOtherAttrs(fullItem.getGroupedOtherAttributes());
         itemAttribute.setSkuAttrs(fullItem.getGroupedSkuAttributes());
         String richText = this.itemDetailDao.findByItemId(item.getId()).getDetail();
         ItemDetail itemDetail = fullItem.getItemDetail();
         if(itemDetail == null) {
            itemDetail = new ItemDetail();
         }

         itemDetail.setItemId(item.getId());
         itemDetail.setDetail(richText);
         String itemInfoMd5 = Digestors.itemDigest(item, itemDetail, itemAttribute);
         item.setItemInfoMd5(itemInfoMd5);
         this.itemManager.updateItem(item, itemDetail, itemAttribute, fullItem.getSkus());
         return Response.ok(Boolean.TRUE);
      } catch (Exception var7) {
         log.error("failed to update {}, cause:{}", fullItem, Throwables.getStackTraceAsString(var7));
         return Response.fail("item.update.fail");
      }
   }

   public Response delete(Long shopId, Long itemId) {
      try {
         this.itemManager.delete(shopId, itemId);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("failed to delete item(id={}) of shop(id={}), cause:{}", new Object[]{itemId, shopId, Throwables.getStackTraceAsString(var4)});
         return Response.fail("item.delete.fail");
      }
   }

   public Response updateStatusByShopIdAndItemId(Long shopId, Long itemId, Integer status) {
      try {
         this.itemManager.updateStatusByShopIdAndItemId(shopId, itemId, status);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var5) {
         log.error("failed to update status of item(id={}) of shop(id={}), cause:{}", new Object[]{itemId, shopId, Throwables.getStackTraceAsString(var5)});
         return Response.fail("item.update.fail");
      }
   }

   public Response updateStatusByItemId(Long itemId, Integer status) {
      try {
         this.itemManager.updateStatusByItemId(itemId, status);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("failed to update status of item(id={}), cause:{}", itemId, Throwables.getStackTraceAsString(var4));
         return Response.fail("item.update.fail");
      }
   }

   public Response batchUpdateStatusByShopIdAndItemIds(Long shopId, List ids, Integer status) {
      try {
         this.itemManager.batchUpdateStatusByShopIdAndItemIds(shopId, ids, status);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var5) {
         log.error("failed to update status of items(ids={}) of shop(id={}), cause:{}", new Object[]{ids, shopId, Throwables.getStackTraceAsString(var5)});
         return Response.fail("item.update.fail");
      }
   }

   public Response editRichText(Long itemId, String richText) {
      try {
         ItemDetail itemDetail = this.itemDetailDao.findByItemId(itemId);
         itemDetail.setDetail(richText);
         Item item = (Item)this.itemDao.findById(itemId);
         ItemAttribute itemAttribute = this.itemAttributeDao.findByItemId(itemId);
         String itemInfoMd5 = Digestors.itemDigest(item, itemDetail, itemAttribute);
         item.setItemInfoMd5(itemInfoMd5);
         this.itemManager.updateRichText(itemInfoMd5, itemDetail);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var7) {
         log.error("failed to edit rich text for item(id={}), cause:{}", itemId, Throwables.getStackTraceAsString(var7));
         return Response.fail("item.detail.update.fail");
      }
   }

   public Response updateDigest(Long itemId, String digest) {
      try {
         this.itemDao.updateItemInfoMd5(itemId, digest);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("failed to update item info md5 for item(id={}), cause:{}", itemId, Throwables.getStackTraceAsString(var4));
         return Response.fail("item.update.fail");
      }
   }
}
