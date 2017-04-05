package io.terminus.parana.web.core.events.shop;

import java.io.Serializable;

public class ShopFrozenEvent implements Serializable {
   private static final long serialVersionUID = 6831669190972056397L;
   private final Long shopId;
   private final Long operatorId;

   public ShopFrozenEvent(Long shopId, Long operatorId) {
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
