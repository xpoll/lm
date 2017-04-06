package cn.blmdz.wolf.web.core.events.spu;

import cn.blmdz.wolf.web.core.events.spu.SpuEvent;

public class SpuDeletedEvent extends SpuEvent {
   private static final long serialVersionUID = 8815303114700392719L;

   public SpuDeletedEvent(Long spuId) {
      super(spuId);
   }
}
