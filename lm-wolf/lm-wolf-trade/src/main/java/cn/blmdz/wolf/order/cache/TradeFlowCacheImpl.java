package cn.blmdz.wolf.order.cache;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.order.cache.TradeFlowCache;
import cn.blmdz.wolf.order.dao.OrderActionInstanceDao;
import cn.blmdz.wolf.order.dao.OrderNodeInstanceDao;
import cn.blmdz.wolf.order.model.OrderActionInstance;
import cn.blmdz.wolf.order.model.OrderNodeInstance;

@Service
public class TradeFlowCacheImpl implements TradeFlowCache {
   private static final Logger log = LoggerFactory.getLogger(TradeFlowCacheImpl.class);
   private final LoadingCache<Long, OrderActionInstance> actionsCache;
   private final LoadingCache<Long, OrderNodeInstance> nodeInstanceCache;

   @Autowired
   public TradeFlowCacheImpl(final OrderActionInstanceDao orderActionInstanceDao, final OrderNodeInstanceDao orderNodeInstanceDao) {
      this.nodeInstanceCache = CacheBuilder.newBuilder().build(new CacheLoader<Long, OrderNodeInstance>() {
         public OrderNodeInstance load(Long id) throws Exception {
            return (OrderNodeInstance)orderNodeInstanceDao.findById(id);
         }
      });
      this.actionsCache = CacheBuilder.newBuilder().build(new CacheLoader<Long, OrderActionInstance>() {
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
