package io.terminus.parana.web.core.events.spu;

import io.terminus.parana.web.core.events.spu.SpuEvent;

public class SpuUpdatedEvent extends SpuEvent {
   private static final long serialVersionUID = -890812736569822297L;

   public SpuUpdatedEvent(Long spuId) {
      super(spuId);
   }
}
