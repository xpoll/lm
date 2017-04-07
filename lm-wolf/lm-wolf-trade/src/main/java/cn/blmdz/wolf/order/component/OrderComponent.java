package cn.blmdz.wolf.order.component;

import java.io.Serializable;
import java.util.Map;

import cn.blmdz.wolf.order.model.Order;

public interface OrderComponent {
   void detectOrderTypeAndCreate(Order order);

   void createOrderExtra(Map<String, Serializable> context, Order order);

   void detectOrderTypeAndUpdate(Order order);

   void updateOrderExtra(Map<String, Serializable> context);

   Order detectOrderTypeAndQuery(Order order);
}
