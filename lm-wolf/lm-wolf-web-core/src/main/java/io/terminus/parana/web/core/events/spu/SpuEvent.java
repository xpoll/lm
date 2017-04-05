package io.terminus.parana.web.core.events.spu;

import java.io.Serializable;

public class SpuEvent implements Serializable {
   private static final long serialVersionUID = -2497411450927503831L;
   protected final Long spuId;

   protected SpuEvent(Long spuId) {
      this.spuId = spuId;
   }

   public Long getSpuId() {
      return this.spuId;
   }
}
