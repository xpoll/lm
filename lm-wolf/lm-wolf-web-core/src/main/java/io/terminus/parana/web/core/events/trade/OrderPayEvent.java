package io.terminus.parana.web.core.events.trade;

import java.io.Serializable;

public class OrderPayEvent implements Serializable {
   private static final long serialVersionUID = 1552214645077182559L;
   private final Long orderId;

   public OrderPayEvent(Long orderId) {
      this.orderId = orderId;
   }

   public Long getOrderId() {
      return this.orderId;
   }
}
