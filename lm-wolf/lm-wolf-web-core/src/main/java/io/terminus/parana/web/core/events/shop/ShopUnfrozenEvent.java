package io.terminus.parana.web.core.events.shop;

import java.io.Serializable;

public class ShopUnfrozenEvent implements Serializable {
   private final Long shopId;
   private final Long operatorId;

   public ShopUnfrozenEvent(Long shopId, Long operatorId) {
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
