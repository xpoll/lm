package cn.blmdz.wolf.web.core.events.item;

import cn.blmdz.wolf.web.core.events.item.ItemEvent;

public class ItemUpdateEvent extends ItemEvent {
   public ItemUpdateEvent(Long itemId) {
      super(itemId);
   }
}
