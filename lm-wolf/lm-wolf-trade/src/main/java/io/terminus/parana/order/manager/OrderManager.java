package io.terminus.parana.order.manager;

import io.terminus.common.exception.ServiceException;
import io.terminus.parana.order.component.OrderComponent;
import io.terminus.parana.order.dao.OrderTransferRuleDao;
import io.terminus.parana.order.dto.OrderChain;
import io.terminus.parana.order.model.Order;
import io.terminus.parana.order.model.OrderTransferRule;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
public class OrderManager {
   private final OrderComponent orderComponent;
   private final OrderTransferRuleDao orderTransferRuleDao;
   private static final Long VIRTUAL_ID = Long.valueOf(-1L);
   private static final Integer VIRTUAL_TYPE = Integer.valueOf(-1);

   @Autowired
   public OrderManager(OrderComponent orderComponent, OrderTransferRuleDao orderTransferRuleDao) {
      this.orderComponent = orderComponent;
      this.orderTransferRuleDao = orderTransferRuleDao;
   }

   @Transactional
   public void deepCreateOrder(OrderChain root, Long parentId, Integer parentType, List orderIds) {
      Order current = root.getOrder();
      if(null != current) {
         current.setParentId(parentId);
         current.setParentType(parentType);
         this.orderComponent.detectOrderTypeAndCreate(current);
         this.orderComponent.createOrderExtra(root.getContext(), current);
         if(Objects.equals(parentId, VIRTUAL_ID)) {
            orderIds.add(current.getId());
         }
      }

      if(root.hasChildren()) {
         for(OrderChain orderChain : root.getChildren()) {
            if(null != current) {
               this.deepCreateOrder(orderChain, current.getId(), current.getType(), orderIds);
            } else {
               this.deepCreateOrder(orderChain, VIRTUAL_ID, VIRTUAL_TYPE, orderIds);
            }
         }
      }

   }

   @Transactional
   public Long create(Order order, Map context) {
      this.orderComponent.detectOrderTypeAndCreate(order);
      this.orderComponent.createOrderExtra(context, order);
      return order.getId();
   }

   @Transactional
   public void update(Order order, Map context) {
      this.checkOrderStatus(order);
      this.orderComponent.detectOrderTypeAndUpdate(order);
      this.orderComponent.updateOrderExtra(context);
   }

   @Transactional
   public void batchUpdate(List orders, Map context) {
      for(Order order : orders) {
         this.checkOrderStatus(order);
         this.orderComponent.detectOrderTypeAndUpdate(order);
      }

      this.orderComponent.updateOrderExtra(context);
   }

   private void checkOrderStatus(Order order) {
      Order beforeOrder = this.orderComponent.detectOrderTypeAndQuery(order);
      Long startNodeInstanceId = beforeOrder.getNodeInstanceId();
      Long endNodeInstanceId = order.getNodeInstanceId();
      if(!Objects.equals(startNodeInstanceId, endNodeInstanceId)) {
         OrderTransferRule orderTransferRule = this.orderTransferRuleDao.findByStartAndEnd(startNodeInstanceId, endNodeInstanceId);
         if(null == orderTransferRule) {
            throw new ServiceException("order.status.check.fail");
         }
      }
   }
}
