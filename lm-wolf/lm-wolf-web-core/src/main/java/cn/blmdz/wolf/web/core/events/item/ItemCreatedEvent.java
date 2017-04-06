package cn.blmdz.wolf.web.core.events.item;

import cn.blmdz.wolf.web.core.events.item.ItemEvent;

public class ItemCreatedEvent extends ItemEvent {
   public ItemCreatedEvent(Long itemId) {
      super(itemId);
   }

   public String toString() {
      return "ItemCreatedEvent()";
   }
}
