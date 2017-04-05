package io.terminus.parana.order.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.order.dao.OrderJobDataDao;
import io.terminus.parana.order.model.OrderJobData;
import io.terminus.parana.order.service.OrderJobDataWriteService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class OrderJobDataWriteServiceImpl implements OrderJobDataWriteService {
   private static final Logger log = LoggerFactory.getLogger(OrderJobDataWriteServiceImpl.class);
   private final OrderJobDataDao orderJobDataDao;

   @Autowired
   public OrderJobDataWriteServiceImpl(OrderJobDataDao orderJobDataDao) {
      this.orderJobDataDao = orderJobDataDao;
   }

   public Response create(OrderJobData orderJobData) {
      try {
         this.orderJobDataDao.create(orderJobData);
         return Response.ok(orderJobData.getId());
      } catch (Exception var3) {
         log.error("fail to create order job data by {}, cause:{}", orderJobData, Throwables.getStackTraceAsString(var3));
         return Response.fail("order.job.data.create.fail");
      }
   }

   public Response update(OrderJobData orderJobData) {
      try {
         this.orderJobDataDao.update(orderJobData);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("fail to update order job data by {}, cause:{}", orderJobData, Throwables.getStackTraceAsString(var3));
         return Response.fail("order.job.data.update.fail");
      }
   }

   public Response updateBy(Long orderId, Integer orderType, Long actionInstanceId, Integer status) {
      try {
         if(orderId != null && orderType != null && actionInstanceId != null && status != null) {
            this.orderJobDataDao.updateBy(orderId, orderType, actionInstanceId, status);
            return Response.ok(Boolean.TRUE);
         } else {
            return Response.fail("invalid.arguments");
         }
      } catch (Exception var6) {
         log.error("fail to update order by order id {}, order type {}, action instance id {}, status {}, cause:{}", new Object[]{orderId, orderType, actionInstanceId, status, Throwables.getStackTraceAsString(var6)});
         return Response.fail("order.job.data.update.fail");
      }
   }
}
