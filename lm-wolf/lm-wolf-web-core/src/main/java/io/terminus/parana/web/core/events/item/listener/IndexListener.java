package io.terminus.parana.web.core.events.item.listener;

import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;
import io.terminus.common.model.Response;
import io.terminus.parana.search.item.ItemSearchWriteService;
import io.terminus.parana.web.core.events.item.ItemDeletedEvent;
import io.terminus.parana.web.core.events.item.ItemUpdateEvent;
import javax.annotation.PostConstruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class IndexListener {
   private static final Logger log = LoggerFactory.getLogger(IndexListener.class);
   private final ItemSearchWriteService itemSearchWriteService;
   private final EventBus eventBus;

   public IndexListener(ItemSearchWriteService itemSearchWriteService, EventBus eventBus) {
      this.itemSearchWriteService = itemSearchWriteService;
      this.eventBus = eventBus;
   }

   @PostConstruct
   public void init() {
      this.eventBus.register(this);
   }

   @Subscribe
   public void onItemUpdate(ItemUpdateEvent itemUpdateEvent) {
      Long itemId = itemUpdateEvent.getItemId();
      Response<Boolean> response = this.itemSearchWriteService.update(itemId);
      if(!response.isSuccess()) {
         log.error("failed to update item(id={}) index, error code:{}", itemId, response.getError());
      }

   }

   @Subscribe
   public void onItemDeleted(ItemDeletedEvent itemDeletedEvent) {
      Long itemId = itemDeletedEvent.getItemId();
      Response<Boolean> response = this.itemSearchWriteService.delete(itemId);
      if(!response.isSuccess()) {
         log.error("failed to delete item(id={}) index, error code:{}", itemId, response.getError());
      }

   }
}
