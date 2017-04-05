package io.terminus.parana.order.cache;

import com.google.common.base.Throwables;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import io.terminus.common.model.Response;
import io.terminus.parana.order.cache.TradeFlowCache;
import io.terminus.parana.order.dao.OrderActionInstanceDao;
import io.terminus.parana.order.dao.OrderNodeInstanceDao;
import io.terminus.parana.order.model.OrderActionInstance;
import io.terminus.parana.order.model.OrderNodeInstance;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class TradeFlowCacheImpl implements TradeFlowCache {
   private static final Logger log = LoggerFactory.getLogger(TradeFlowCacheImpl.class);
   private final LoadingCache actionsCache;
   private final LoadingCache nodeInstanceCache;

   @Autowired
   public TradeFlowCacheImpl(final OrderActionInstanceDao orderActionInstanceDao, final OrderNodeInstanceDao orderNodeInstanceDao) {
      this.nodeInstanceCache = CacheBuilder.newBuilder().build(new CacheLoader() {
         public OrderNodeInstance load(Long id) throws Exception {
            return (OrderNodeInstance)orderNodeInstanceDao.findById(id);
         }
      });
      this.actionsCache = CacheBuilder.newBuilder().build(new CacheLoader() {
         public OrderActionInstance load(Long id) throws Exception {
            return (OrderActionInstance)orderActionInstanceDao.findById(id);
         }
      });
   }

   public Response getOrderActionInstance(Long actionInstanceId) {
      try {
         return Response.ok(this.actionsCache.get(actionInstanceId));
      } catch (Exception var3) {
         log.error("fail to find cached order action instance by id {}, cause:{}", actionInstanceId, Throwables.getStackTraceAsString(var3));
         return Response.fail("action.instance.query.fail");
      }
   }

   public Response getOrderNodeInstance(Long nodeInstanceId) {
      try {
         return Response.ok(this.nodeInstanceCache.get(nodeInstanceId));
      } catch (Exception var3) {
         log.error("fail to find cached order node instance by id {}, cause:{}", nodeInstanceId, Throwables.getStackTraceAsString(var3));
         return Response.fail("node.instance.query.fail");
      }
   }
}
