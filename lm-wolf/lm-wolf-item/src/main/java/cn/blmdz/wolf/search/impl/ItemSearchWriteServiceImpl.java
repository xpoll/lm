package cn.blmdz.wolf.search.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.search.api.IndexExecutor;
import cn.blmdz.wolf.item.impl.dao.ItemAttributeDao;
import cn.blmdz.wolf.item.impl.dao.ItemDao;
import cn.blmdz.wolf.parana.item.model.Item;
import cn.blmdz.wolf.parana.item.model.ItemAttribute;
import cn.blmdz.wolf.parana.search.dto.IndexedItem;
import cn.blmdz.wolf.parana.search.item.ItemSearchWriteService;

@Service
public class ItemSearchWriteServiceImpl implements ItemSearchWriteService {
   private static final Logger log = LoggerFactory.getLogger(ItemSearchWriteServiceImpl.class);
   private final IndexExecutor indexExecutor;
   private final BaseIndexedItemFactory indexedItemFactory;
   private final ItemDao itemDao;
   private final ItemAttributeDao itemAttributeDao;
   private final IndexedItemIndexAction indexedItemIndexAction;

   @Autowired
   public ItemSearchWriteServiceImpl(IndexExecutor indexExecutor, BaseIndexedItemFactory indexedItemFactory, ItemDao itemDao, ItemAttributeDao itemAttributeDao, IndexedItemIndexAction indexedItemIndexAction) {
      this.indexExecutor = indexExecutor;
      this.indexedItemFactory = indexedItemFactory;
      this.itemDao = itemDao;
      this.itemAttributeDao = itemAttributeDao;
      this.indexedItemIndexAction = indexedItemIndexAction;
   }

   public Response index(Long itemId) {
      try {
         Item item = (Item)this.itemDao.findById(itemId);
         ItemAttribute itemAttribute = this.itemAttributeDao.findByItemId(itemId);
         IndexedItem indexedItem = this.indexedItemFactory.create(item, itemAttribute, new Object[0]);
         this.indexExecutor.submit(this.indexedItemIndexAction.indexTask(indexedItem));
         return Response.ok(Boolean.TRUE);
      } catch (Exception var5) {
         log.error("failed to index item(id={}), cause:{}", itemId, Throwables.getStackTraceAsString(var5));
         return Response.fail("item.index.fail");
      }
   }

   public Response delete(Long itemId) {
      try {
         this.indexExecutor.submit(this.indexedItemIndexAction.deleteTask(itemId));
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to delete item(id={}) from index, cause:{}", itemId, Throwables.getStackTraceAsString(var3));
         return Response.fail("item.index.fail");
      }
   }

   public Response update(Long itemId) {
      try {
         Item item = (Item)this.itemDao.findById(itemId);
         if(item.getStatus().intValue() > 0) {
            ItemAttribute itemAttribute = this.itemAttributeDao.findByItemId(itemId);
            IndexedItem indexedItem = this.indexedItemFactory.create(item, itemAttribute, new Object[0]);
            this.indexExecutor.submit(this.indexedItemIndexAction.indexTask(indexedItem));
            return Response.ok(Boolean.TRUE);
         } else {
            this.indexExecutor.submit(this.indexedItemIndexAction.deleteTask(itemId));
            return Response.ok(Boolean.TRUE);
         }
      } catch (Exception var5) {
         log.error("failed to update item(id={}) from index, cause:{}", itemId, Throwables.getStackTraceAsString(var5));
         return Response.fail("item.index.fail");
      }
   }
}
