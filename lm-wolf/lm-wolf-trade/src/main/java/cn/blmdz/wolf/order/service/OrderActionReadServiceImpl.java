package cn.blmdz.wolf.order.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Lists;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.order.dao.OrderActionInstanceDao;
import cn.blmdz.wolf.order.model.OrderActionInstance;
import cn.blmdz.wolf.order.service.OrderActionReadService;

@Service
public class OrderActionReadServiceImpl implements OrderActionReadService {
   private static final Logger log = LoggerFactory.getLogger(OrderActionReadServiceImpl.class);
   private final OrderActionInstanceDao orderActionInstanceDao;
   private static final JsonMapper JSON_MAPPER = JsonMapper.nonDefaultMapper();

   @Autowired
   public OrderActionReadServiceImpl(OrderActionInstanceDao orderActionInstanceDao) {
      this.orderActionInstanceDao = orderActionInstanceDao;
   }

   public Response findActionInstancesByActionType(Long nodeInstanceId, Integer actionType) {
      try {
         List<OrderActionInstance> actionInstances = this.orderActionInstanceDao.findByNsidAndType(nodeInstanceId, actionType);
         return Response.ok(actionInstances);
      } catch (Exception var4) {
         log.error("fail to find next action instance by node instance id {}, action type, cause:{}", new Object[]{nodeInstanceId, actionType, Throwables.getStackTraceAsString(var4)});
         return Response.fail("query.action.fail");
      }
   }

   public Response findExcludedActionsGroupByUserType(Long nodeInstanceId, Long... excludeActionIds) {
      try {
         List<OrderActionInstance> actionInstances = this.orderActionInstanceDao.findByNsid(nodeInstanceId);
         List<OrderActionInstance> excludedActionInstances = this.excludeActionInstance(actionInstances, excludeActionIds);
         ArrayListMultimap<Integer, OrderActionInstance> grouped = ArrayListMultimap.create();

         for(OrderActionInstance orderActionInstance : excludedActionInstances) {
            for(Integer userType : (List<Integer>)JSON_MAPPER.fromJson(orderActionInstance.getBelongUserTypes(), JSON_MAPPER.createCollectionType(List.class, new Class[]{Integer.class}))) {
               grouped.put(userType, orderActionInstance);
            }
         }

         Map<Integer, List<OrderActionInstance>> map = new HashMap();

         for(Integer userType : grouped.keySet()) {
            List<OrderActionInstance> userTypeActions = new ArrayList();
            userTypeActions.addAll(grouped.get(userType));
            map.put(userType, userTypeActions);
         }

         return Response.ok(map);
      } catch (Exception var11) {
         log.error("fail to find action instance by node instance id {}, cause:{}", nodeInstanceId, Throwables.getStackTraceAsString(var11));
         return Response.fail("order.action.query.fail");
      }
   }

   private List excludeActionInstance(List<OrderActionInstance> actionInstances, Long... excludeIds) {
      List<OrderActionInstance> excluded = new ArrayList();

      for(OrderActionInstance orderActionInstance : actionInstances) {
         if(!Lists.newArrayList(excludeIds).contains(orderActionInstance.getId()) && orderActionInstance.getDisplay().booleanValue()) {
            excluded.add(orderActionInstance);
         }
      }

      return excluded;
   }

   public Response findById(Long actionInstanceId) {
      try {
         OrderActionInstance actionInstance = (OrderActionInstance)this.orderActionInstanceDao.findById(actionInstanceId);
         if(null == actionInstance) {
            log.error("action instance id={} not found", actionInstanceId);
            return Response.fail("order.action.not.found");
         } else {
            return Response.ok(actionInstance);
         }
      } catch (Exception var3) {
         log.error("fail to find action instance by id {}, cause:{}", actionInstanceId, Throwables.getStackTraceAsString(var3));
         return Response.fail("order.action.query.fail");
      }
   }
}
