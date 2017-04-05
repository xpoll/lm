package io.terminus.parana.web.front.item;

import com.google.common.base.MoreObjects;
import com.google.common.base.Objects;
import com.google.common.collect.Lists;
import com.google.common.eventbus.EventBus;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.pampas.common.UserUtil;
import io.terminus.parana.common.model.ParanaUser;
import io.terminus.parana.component.item.component.ItemWriter;
import io.terminus.parana.item.dto.FullItem;
import io.terminus.parana.item.model.Item;
import io.terminus.parana.item.model.Sku;
import io.terminus.parana.item.service.ItemReadService;
import io.terminus.parana.item.service.ItemWriteService;
import io.terminus.parana.shop.model.Shop;
import io.terminus.parana.shop.service.ShopReadService;
import io.terminus.parana.web.core.events.item.ItemCreatedEvent;
import io.terminus.parana.web.core.events.item.ItemUpdateEvent;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping({"/api"})
public class Items {
   private static final Logger log = LoggerFactory.getLogger(Items.class);
   private final ItemWriter itemWriter;
   private final ShopReadService shopReadService;
   private final ItemReadService itemReadService;
   private final ItemWriteService itemWriteService;
   private final EventBus eventBus;

   @Autowired
   public Items(ItemWriter itemWriter, ShopReadService shopReadService, ItemReadService itemReadService, ItemWriteService itemWriteService, EventBus eventBus) {
      this.itemWriter = itemWriter;
      this.shopReadService = shopReadService;
      this.itemReadService = itemReadService;
      this.itemWriteService = itemWriteService;
      this.eventBus = eventBus;
   }

   @RequestMapping(
      value = {"/seller/items"},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public Long create(@RequestBody FullItem fullItem) {
      ParanaUser paranaUser = (ParanaUser)UserUtil.getCurrentUser();
      Long shopId = paranaUser.getShopId();
      Response<Shop> rShop = this.shopReadService.findById(shopId);
      if(!rShop.isSuccess()) {
         log.error("failed to find shop(id={}), error code:{}", shopId, rShop.getError());
         throw new JsonResponseException(rShop.getError());
      } else {
         Shop shop = (Shop)rShop.getResult();
         if(!Objects.equal(shop.getStatus(), Integer.valueOf(1))) {
            log.error("shop(id={})\'s status is {}, create item failed", shopId);
            throw new JsonResponseException("shop.status.abnormal");
         } else {
            Item item = fullItem.getItem();
            item.setShopId(shopId);
            item.setShopName(shop.getName());
            item.setTags((Map)null);
            this.defaultItemInfos(item);

            try {
               this.extractInfoFromSkus(item, fullItem.getSkus());
            } catch (Exception var8) {
               log.error("bad sku info", var8);
               throw new JsonResponseException("illegal.sku.info");
            }

            Response<Long> rItemId = this.itemWriter.create(fullItem);
            if(!rItemId.isSuccess()) {
               log.error("failed to create {}, error code:{}", fullItem, rItemId.getError());
               throw new JsonResponseException(rItemId.getError());
            } else {
               this.eventBus.post(new ItemCreatedEvent((Long)rItemId.getResult()));
               return (Long)rItemId.getResult();
            }
         }
      }
   }

   private void defaultItemInfos(Item item) {
      item.setStatus((Integer)MoreObjects.firstNonNull(item.getStatus(), Integer.valueOf(-1)));
      item.setSaleQuantity((Integer)MoreObjects.firstNonNull(item.getSaleQuantity(), Integer.valueOf(0)));
      item.setType((Integer)MoreObjects.firstNonNull(item.getType(), Integer.valueOf(1)));
      item.setReduceStockType((Integer)MoreObjects.firstNonNull(item.getReduceStockType(), Integer.valueOf(1)));
      item.setStockType((Integer)MoreObjects.firstNonNull(item.getStockType(), Integer.valueOf(0)));
   }

   private void extractInfoFromSkus(Item item, List skus) {
      if(Objects.equal(item.getStockType(), Integer.valueOf(0))) {
         int stockQuantity = 0;

         for(Sku sku : skus) {
            if(sku.getStockQuantity() == null) {
               throw new JsonResponseException("stock.empty");
            }

            if(sku.getStockQuantity().intValue() < 0) {
               throw new IllegalArgumentException("sku.stock.negative");
            }

            stockQuantity += sku.getStockQuantity().intValue();
         }

         item.setStockQuantity(Integer.valueOf(stockQuantity));
      }

      int highPrice = -1;
      int lowPrice = -1;

      for(Sku sku : skus) {
         if(sku.getPrice() != null) {
            if(sku.getPrice().intValue() <= 0) {
               throw new IllegalArgumentException("sku.price.need.positive");
            }

            if(sku.getPrice().intValue() > highPrice) {
               highPrice = sku.getPrice().intValue();
            }

            if(sku.getPrice().intValue() < lowPrice || lowPrice < 0) {
               lowPrice = sku.getPrice().intValue();
            }
         }
      }

      if(highPrice > 0) {
         item.setHighPrice(Integer.valueOf(highPrice));
      }

      if(lowPrice > 0) {
         item.setLowPrice(Integer.valueOf(lowPrice));
      }

   }

   @RequestMapping(
      value = {"/seller/items"},
      method = {RequestMethod.PUT},
      produces = {"application/json"}
   )
   public Boolean update(@RequestBody FullItem fullItem) {
      ParanaUser paranaUser = (ParanaUser)UserUtil.getCurrentUser();
      Long shopId = paranaUser.getShopId();
      Response<Shop> rShop = this.shopReadService.findById(shopId);
      if(!rShop.isSuccess()) {
         log.error("failed to find shop(id={}), error code:{}", shopId, rShop.getError());
         throw new JsonResponseException(rShop.getError());
      } else {
         Shop shop = (Shop)rShop.getResult();
         if(!Objects.equal(shop.getStatus(), Integer.valueOf(1))) {
            log.error("shop(id={})\'s status is {}, create item failed", shopId);
            throw new JsonResponseException("shop.status.abnormal");
         } else {
            Item item = fullItem.getItem();
            item.setShopId(shopId);
            item.setShopName(shop.getName());
            item.setTags((Map)null);
            Long itemId = item.getId();
            Response<Item> rItem = this.itemReadService.findById(itemId);
            if(!rItem.isSuccess()) {
               log.error("failed to find item(id={}), error code:{}", itemId, rItem.getError());
               throw new JsonResponseException(rItem.getError());
            } else {
               item.setStockType(((Item)rItem.getResult()).getStockType());
               this.extractInfoFromSkus(item, fullItem.getSkus());
               Response<Boolean> rUpdate = this.itemWriter.update(fullItem);
               if(!rUpdate.isSuccess()) {
                  log.error("failed to update {}, error code:{}", fullItem, rUpdate.getError());
                  throw new JsonResponseException(rUpdate.getError());
               } else {
                  this.eventBus.post(new ItemUpdateEvent(itemId));
                  return (Boolean)rUpdate.getResult();
               }
            }
         }
      }
   }

   @RequestMapping(
      value = {"/seller/items/{id}/status"},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public Boolean updateStatus(@PathVariable("id") Long itemId, @RequestParam("status") Integer status) {
      ParanaUser paranaUser = (ParanaUser)UserUtil.getCurrentUser();
      Response<Boolean> r = this.itemWriteService.updateStatusByShopIdAndItemId(paranaUser.getShopId(), itemId, status);
      if(!r.isSuccess()) {
         log.error("failed to update status to {} for item(id={}), error code:{}", new Object[]{status, itemId, r.getError()});
         throw new JsonResponseException(r.getError());
      } else {
         this.eventBus.post(new ItemUpdateEvent(itemId));
         return (Boolean)r.getResult();
      }
   }

   @RequestMapping(
      value = {"/seller/items/status"},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public Boolean batchUpdateStatus(@RequestParam("ids[]") Long[] ids, @RequestParam Integer status) {
      ParanaUser paranaUser = (ParanaUser)UserUtil.getCurrentUser();
      List<Long> itemIds = Arrays.asList(ids);
      Response<Boolean> r = this.itemWriteService.batchUpdateStatusByShopIdAndItemIds(paranaUser.getShopId(), itemIds, status);
      if(!r.isSuccess()) {
         log.error("failed to update status to {} for item(id={}), error code:{}", new Object[]{status, itemIds, r.getError()});
         throw new JsonResponseException(r.getError());
      } else {
         for(Long id : ids) {
            this.eventBus.post(new ItemUpdateEvent(id));
         }

         return (Boolean)r.getResult();
      }
   }

   @RequestMapping(
      value = {"/mobile/item/{id}"},
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public FullItem findById(@PathVariable("id") Long id) {
      Response<FullItem> rFullItem = this.itemReadService.findFullInfoByItemId(id);
      if(!rFullItem.isSuccess()) {
         log.error("failed to find full item(id={}), error code:{}", id, rFullItem.getError());
         throw new JsonResponseException(rFullItem.getError());
      } else {
         return (FullItem)rFullItem.getResult();
      }
   }

   @RequestMapping(
      value = {"/items"},
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public List findItemsByIds(@RequestParam Long[] ids) {
      if(ids != null && ids.length != 0) {
         Response<List<Item>> resp = this.itemReadService.findByIds(Lists.newArrayList(ids));
         if(!resp.isSuccess()) {
            log.error("find items by ids failed, ids={}, error={}", ids, resp.getError());
            throw new JsonResponseException(resp.getError());
         } else {
            return (List)resp.getResult();
         }
      } else {
         return Collections.emptyList();
      }
   }
}
