package io.terminus.parana.web.core.events.trade;

import java.io.Serializable;

public class OrderCreatedEvent implements Serializable {
   private static final long serialVersionUID = 6068457279336231160L;
   private final Long orderId;
   private final Long itemId;

   public OrderCreatedEvent(Long orderId, Long itemId) {
      this.orderId = orderId;
      this.itemId = itemId;
   }

   public Long getOrderId() {
      return this.orderId;
   }

   public Long getItemId() {
      return this.itemId;
   }
}
