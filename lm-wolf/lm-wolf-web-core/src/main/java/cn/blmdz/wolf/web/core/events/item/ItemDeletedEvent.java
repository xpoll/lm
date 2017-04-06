package cn.blmdz.wolf.web.core.events.item;

import cn.blmdz.wolf.web.core.events.item.ItemEvent;

public class ItemDeletedEvent extends ItemEvent {
   public ItemDeletedEvent(Long itemId) {
      super(itemId);
   }
}
