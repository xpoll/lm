package io.terminus.parana.item.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.item.common.Digestors;
import io.terminus.parana.item.impl.dao.ItemAttributeDao;
import io.terminus.parana.item.impl.dao.ItemDao;
import io.terminus.parana.item.impl.dao.ItemDetailDao;
import io.terminus.parana.item.model.Item;
import io.terminus.parana.item.model.ItemAttribute;
import io.terminus.parana.item.model.ItemDetail;
import io.terminus.parana.item.service.AdminItemWriteService;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class AdminItemWriteServiceImpl implements AdminItemWriteService {
   private static final Logger log = LoggerFactory.getLogger(AdminItemWriteServiceImpl.class);
   private final ItemDao itemDao;
   private final ItemDetailDao itemDetailDao;
   private final ItemAttributeDao itemAttributeDao;

   @Autowired
   public AdminItemWriteServiceImpl(ItemDao itemDao, ItemDetailDao itemDetailDao, ItemAttributeDao itemAttributeDao) {
      this.itemDao = itemDao;
      this.itemDetailDao = itemDetailDao;
      this.itemAttributeDao = itemAttributeDao;
   }

   public Response batchUpdateStatusByShopId(Long shopId, Integer status) {
      try {
         this.itemDao.batchUpdateStatusByShopId(shopId, status);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("failed to update item(shopId={}) to target status({}),cause:{}", new Object[]{shopId, status, Throwables.getStackTraceAsString(var4)});
         return Response.fail("item.update.fail");
      }
   }

   public Response batchUpdateStatus(List ids, Integer status) {
      try {
         this.itemDao.batchUpdateStatus(ids, status);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("failed to update items(ids={}) to target status({}),cause:{}", new Object[]{ids, status, Throwables.getStackTraceAsString(var4)});
         return Response.fail("item.update.fail");
      }
   }

   public Response updateStatus(Long id, Integer status) {
      try {
         this.itemDao.updateStatus(id, status);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("failed to update item(id={}) to target status({}),cause:{}", new Object[]{id, status, Throwables.getStackTraceAsString(var4)});
         return Response.fail("item.update.fail");
      }
   }

   public Response tags(Long itemId, Map tags) {
      try {
         ItemDetail itemDetail = this.itemDetailDao.findByItemId(itemId);
         Item item = (Item)this.itemDao.findById(itemId);
         item.setTags(tags);
         ItemAttribute itemAttribute = this.itemAttributeDao.findByItemId(itemId);
         String itemInfoMd5 = Digestors.itemDigest(item, itemDetail, itemAttribute);
         this.itemDao.updateTags(itemId, tags, itemInfoMd5);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var7) {
         log.error("failed to update item(id={}) tags to {}, cause:{}", new Object[]{itemId, tags, Throwables.getStackTraceAsString(var7)});
         return Response.fail("item.update.fail");
      }
   }
}
