package io.terminus.parana.order.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.order.dao.OrderJobDataDao;
import io.terminus.parana.order.model.OrderJobData;
import io.terminus.parana.order.service.OrderJobDataReadService;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class OrderJobDataReadServiceImpl implements OrderJobDataReadService {
   private static final Logger log = LoggerFactory.getLogger(OrderJobDataReadServiceImpl.class);
   private final OrderJobDataDao orderJobDataDao;

   @Autowired
   public OrderJobDataReadServiceImpl(OrderJobDataDao orderJobDataDao) {
      this.orderJobDataDao = orderJobDataDao;
   }

   public Response listTo(Long lastId, Integer limit) {
      try {
         List<OrderJobData> orderJobDatas = this.orderJobDataDao.listTo(lastId, limit);
         return Response.ok(orderJobDatas);
      } catch (Exception var4) {
         log.error("fail to query order job data by lastId {}, limit {}, cause:{}", new Object[]{lastId, limit, Throwables.getStackTraceAsString(var4)});
         return Response.fail("order.job.data.query.fail");
      }
   }

   public Response lastId() {
      try {
         return Response.ok(this.orderJobDataDao.lastId());
      } catch (Exception var2) {
         log.error("fail to get order job data last id, cause:{}", var2);
         return Response.fail("order.job.data.query.fail");
      }
   }
}
