package io.terminus.parana.web.core.events.item;

import io.terminus.parana.web.core.events.item.ItemEvent;

public class ItemUpdateEvent extends ItemEvent {
   public ItemUpdateEvent(Long itemId) {
      super(itemId);
   }
}
