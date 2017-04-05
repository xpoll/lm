package io.terminus.parana.web.core.events.item;

import io.terminus.parana.web.core.events.item.ItemEvent;

public class ItemDeletedEvent extends ItemEvent {
   public ItemDeletedEvent(Long itemId) {
      super(itemId);
   }
}
