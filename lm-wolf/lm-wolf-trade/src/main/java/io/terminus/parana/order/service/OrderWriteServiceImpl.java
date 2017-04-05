package io.terminus.parana.order.service;

import com.google.common.base.Throwables;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.parana.order.dto.OrderChain;
import io.terminus.parana.order.manager.OrderManager;
import io.terminus.parana.order.model.Order;
import io.terminus.parana.order.service.OrderWriteService;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class OrderWriteServiceImpl implements OrderWriteService {
   private static final Logger log = LoggerFactory.getLogger(OrderWriteServiceImpl.class);
   private final OrderManager orderManager;

   @Autowired
   public OrderWriteServiceImpl(OrderManager orderManager) {
      this.orderManager = orderManager;
   }

   public Response create(OrderChain root) {
      try {
         List<Long> orderIds = new ArrayList();
         Long VIRTUAL_ID = Long.valueOf(-1L);
         Integer VIRTUAL_TYPE = Integer.valueOf(-1);
         this.orderManager.deepCreateOrder(root, VIRTUAL_ID, VIRTUAL_TYPE, orderIds);
         return Response.ok(orderIds);
      } catch (ServiceException var5) {
         return Response.fail(var5.getMessage());
      } catch (Exception var6) {
         log.error("fail to create order by orders {}, cause:{}", root, Throwables.getStackTraceAsString(var6));
         return Response.fail("order.create.fail");
      }
   }

   public Response create(Order order, Map context) {
      try {
         this.orderManager.create(order, context);
         return Response.ok(order.getId());
      } catch (ServiceException var4) {
         return Response.fail(var4.getMessage());
      } catch (Exception var5) {
         log.error("fail to create order by order {}, context {}, cause:{}", new Object[]{order, context, Throwables.getStackTraceAsString(var5)});
         return Response.fail("order.create.fail");
      }
   }

   public Response update(List orders, Map context) {
      try {
         this.orderManager.batchUpdate(orders, context);
         return Response.ok(Boolean.TRUE);
      } catch (ServiceException var4) {
         return Response.fail(var4.getMessage());
      } catch (Exception var5) {
         log.error("fail to update order by orders {}, context {}, cause:{}", new Object[]{orders, context, Throwables.getStackTraceAsString(var5)});
         return Response.fail("update.order.fail");
      }
   }

   public Response update(Order order, Map context) {
      try {
         this.orderManager.update(order, context);
         return Response.ok(Boolean.TRUE);
      } catch (ServiceException var4) {
         return Response.fail(var4.getMessage());
      } catch (Exception var5) {
         log.error("fail to update order by order {}, context {}, cause:{}", new Object[]{order, context, Throwables.getStackTraceAsString(var5)});
         return Response.fail("order.update.fail");
      }
   }
}
