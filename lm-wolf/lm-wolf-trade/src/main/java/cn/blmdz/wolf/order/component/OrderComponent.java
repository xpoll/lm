package cn.blmdz.wolf.order.component;

import java.util.Map;

import cn.blmdz.wolf.order.model.Order;

public interface OrderComponent {
   void detectOrderTypeAndCreate(Order var1);

   void createOrderExtra(Map var1, Order var2);

   void detectOrderTypeAndUpdate(Order var1);

   void updateOrderExtra(Map var1);

   Order detectOrderTypeAndQuery(Order var1);
}
