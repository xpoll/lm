package cn.blmdz.wolf.web.admin.item;

import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.google.common.eventbus.EventBus;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.wolf.item.model.Item;
import cn.blmdz.wolf.item.service.AdminItemWriteService;
import cn.blmdz.wolf.item.service.ItemReadService;
import cn.blmdz.wolf.web.core.events.item.ItemUpdateEvent;

@RestController
@RequestMapping({"/api/item"})
public class Items {
   private static final Logger log = LoggerFactory.getLogger(Items.class);
   private final ItemReadService itemReadService;
   private final AdminItemWriteService itemWriteService;
   private final EventBus eventBus;

   @Autowired
   public Items(ItemReadService itemReadService, AdminItemWriteService itemWriteService, EventBus eventBus) {
      this.itemReadService = itemReadService;
      this.itemWriteService = itemWriteService;
      this.eventBus = eventBus;
   }

   @RequestMapping(
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public Item findById(@RequestParam(
   name = "id"
) Long id) {
      Response<Item> rItem = this.itemReadService.findById(id);
      if(!rItem.isSuccess()) {
         log.error("failed to find item(id={}),error code:{}", id, rItem.getError());
         throw new JsonResponseException(rItem.getError());
      } else {
         return (Item)rItem.getResult();
      }
   }

   @RequestMapping(
      value = {"/status"},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public boolean updateStatus(@RequestParam(
   name = "id"
) Long id, @RequestParam(
   name = "status"
) Integer status) {
      Response<Boolean> r = this.itemWriteService.updateStatus(id, status);
      if(!r.isSuccess()) {
         log.error("failed to update status to {} for item(id={}), error code:{} ", new Object[]{status, id, r.getError()});
         throw new JsonResponseException(r.getError());
      } else {
         this.eventBus.post(new ItemUpdateEvent(id));
         return true;
      }
   }

   @RequestMapping(
      value = {"/status/batch"},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public boolean batchUpdateStatus(@RequestParam(
   name = "ids"
) String ids, @RequestParam(
   name = "status"
) Integer status) {
      List<String> parts = Splitters.COMMA.splitToList(ids);
      if(CollectionUtils.isEmpty(parts)) {
         log.warn("no item ids specified, skip");
         return true;
      } else {
         List<Long> itemIds = Lists.newArrayListWithCapacity(parts.size());

         for(String part : parts) {
            itemIds.add(Long.valueOf(Long.parseLong(part)));
         }

         Response<Boolean> r = this.itemWriteService.batchUpdateStatus(itemIds, status);
         if(!r.isSuccess()) {
            log.error("failed to update status to {} for items(ids={}), error code:{} ", new Object[]{status, ids, r.getError()});
            throw new JsonResponseException(r.getError());
         } else {
            for(Long itemId : itemIds) {
               this.eventBus.post(new ItemUpdateEvent(itemId));
            }

            return true;
         }
      }
   }

   @RequestMapping(
      value = {"/tags"},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public boolean updateTags(@RequestParam(
   name = "id"
) Long id, @RequestParam("tags") String tags) {
      Map<String, String> realTags = Splitter.on(',').withKeyValueSeparator(':').split(tags);
      Response<Boolean> r = this.itemWriteService.tags(id, realTags);
      if(!r.isSuccess()) {
         log.error("failed to update tags to {} for item(id={}), error code:{} ", new Object[]{tags, id, r.getError()});
         throw new JsonResponseException(r.getError());
      } else {
         this.eventBus.post(new ItemUpdateEvent(id));
         return true;
      }
   }
}
