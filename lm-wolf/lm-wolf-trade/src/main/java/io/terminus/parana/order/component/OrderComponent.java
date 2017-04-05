package io.terminus.parana.order.component;

import io.terminus.parana.order.model.Order;
import java.util.Map;

public interface OrderComponent {
   void detectOrderTypeAndCreate(Order var1);

   void createOrderExtra(Map var1, Order var2);

   void detectOrderTypeAndUpdate(Order var1);

   void updateOrderExtra(Map var1);

   Order detectOrderTypeAndQuery(Order var1);
}
