package io.terminus.parana.search.impl;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.item.impl.dao.ItemAttributeDao;
import io.terminus.parana.item.impl.dao.ItemDao;
import io.terminus.parana.item.model.Item;
import io.terminus.parana.item.model.ItemAttribute;
import io.terminus.parana.search.dto.IndexedItem;
import io.terminus.parana.search.impl.BaseIndexedItemFactory;
import io.terminus.parana.search.impl.IndexedItemIndexAction;
import io.terminus.parana.search.item.ItemSearchWriteService;
import io.terminus.search.api.IndexExecutor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

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
