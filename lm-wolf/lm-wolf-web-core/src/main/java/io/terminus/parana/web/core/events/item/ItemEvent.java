package io.terminus.parana.web.core.events.item;

import java.io.Serializable;

public abstract class ItemEvent implements Serializable {
   private static final long serialVersionUID = 3036882332919819869L;
   protected final Long itemId;

   protected ItemEvent(Long itemId) {
      this.itemId = itemId;
   }

   public Long getItemId() {
      return this.itemId;
   }
}
