package io.terminus.parana.cache;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;
import io.terminus.parana.item.dto.ViewedItem;
import io.terminus.parana.item.model.Item;
import io.terminus.parana.item.service.ItemReadService;
import java.util.concurrent.TimeUnit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class ItemCacher {
   private static final Logger log = LoggerFactory.getLogger(ItemCacher.class);
   private final LoadingCache itemCache;
   private final LoadingCache viewedItemCache;

   @Autowired
   public ItemCacher(final ItemReadService itemReadService, @Value("${cache.duration:60}") Integer duration) {
      this.itemCache = CacheBuilder.newBuilder().expireAfterWrite((long)duration.intValue(), TimeUnit.MINUTES).build(new CacheLoader() {
         public Item load(Long itemId) throws Exception {
            Response<Item> rItem = itemReadService.findById(itemId);
            if(!rItem.isSuccess()) {
               ItemCacher.log.error("failed to find item(id={}), error code:{}", itemId, rItem.getError());
               throw new ServiceException(rItem.getError());
            } else {
               return (Item)rItem.getResult();
            }
         }
      });
      this.viewedItemCache = CacheBuilder.newBuilder().expireAfterAccess((long)duration.intValue(), TimeUnit.MINUTES).build(new CacheLoader() {
         public ViewedItem load(Long itemId) throws Exception {
            Response<ViewedItem> r = itemReadService.findForView(itemId);
            if(!r.isSuccess()) {
               ItemCacher.log.error("failed to find item(id={}) viewed info, error code:{} ", itemId, r.getError());
               throw new ServiceException(r.getError());
            } else {
               return (ViewedItem)r.getResult();
            }
         }
      });
   }

   public Item findItemById(Long itemId) {
      return (Item)this.itemCache.getUnchecked(itemId);
   }

   @Export(
      paramNames = {"itemId"}
   )
   public ViewedItem findViewedItemById(Long itemId) {
      return (ViewedItem)this.viewedItemCache.getUnchecked(itemId);
   }
}
