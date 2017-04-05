package io.terminus.parana.web.core.events.spu;

import io.terminus.parana.web.core.events.spu.SpuEvent;

public class SpuCreatedEvent extends SpuEvent {
   private static final long serialVersionUID = 5849101828527096666L;

   public SpuCreatedEvent(Long spuId) {
      super(spuId);
   }
}
