package cn.blmdz.wolf.parana.cache;

import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.parana.item.dto.ViewedItem;
import cn.blmdz.wolf.parana.item.model.Item;
import cn.blmdz.wolf.parana.item.service.ItemReadService;

@Component
public class ItemCacher {
   private static final Logger log = LoggerFactory.getLogger(ItemCacher.class);
   private final LoadingCache<Long, Item> itemCache;
   private final LoadingCache<Long, ViewedItem> viewedItemCache;

   @Autowired
   public ItemCacher(final ItemReadService itemReadService, @Value("${cache.duration:60}") Integer duration) {
      this.itemCache = CacheBuilder.newBuilder().expireAfterWrite((long)duration.intValue(), TimeUnit.MINUTES).build(new CacheLoader<Long, Item>() {
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
      this.viewedItemCache = CacheBuilder.newBuilder().expireAfterAccess((long)duration.intValue(), TimeUnit.MINUTES).build(new CacheLoader<Long, ViewedItem>() {
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
