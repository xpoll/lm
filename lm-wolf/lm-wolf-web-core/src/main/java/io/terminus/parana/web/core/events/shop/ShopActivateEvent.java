package io.terminus.parana.web.core.events.shop;

import java.io.Serializable;

public class ShopActivateEvent implements Serializable {
   private static final long serialVersionUID = 3052183949414022083L;
   private final Long shopId;
   private final Long operatorId;

   public ShopActivateEvent(Long shopId, Long operatorId) {
      this.shopId = shopId;
      this.operatorId = operatorId;
   }

   public Long getShopId() {
      return this.shopId;
   }

   public Long getOperatorId() {
      return this.operatorId;
   }
}
