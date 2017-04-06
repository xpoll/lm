package cn.blmdz.wolf.order.service;

import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.order.dao.OrderNodeInstanceDao;
import cn.blmdz.wolf.order.model.OrderNodeInstance;
import cn.blmdz.wolf.order.service.OrderNodeReadService;

@Service
public class OrderNodeReadServiceImpl implements OrderNodeReadService {
   private static final Logger log = LoggerFactory.getLogger(OrderNodeReadServiceImpl.class);
   private final OrderNodeInstanceDao orderNodeInstanceDao;

   @Autowired
   public OrderNodeReadServiceImpl(OrderNodeInstanceDao orderNodeInstanceDao) {
      this.orderNodeInstanceDao = orderNodeInstanceDao;
   }

   public Response getEntranceByFlowId(Long flowId) {
      try {
         OrderNodeInstance entrance = this.orderNodeInstanceDao.getEntranceByFlowId(flowId);
         return null == entrance?Response.fail("flow.entrance.not.found"):Response.ok(entrance);
      } catch (Exception var3) {
         log.error("fail to get entrance by flow id {}, cause:{}", flowId, Throwables.getStackTraceAsString(var3));
         return Response.fail("flow.entrance.query.fail");
      }
   }

   public Response findById(Long nid) {
      try {
         OrderNodeInstance orderNodeInstance = (OrderNodeInstance)this.orderNodeInstanceDao.findById(nid);
         return null == orderNodeInstance?Response.fail("node.instance.not.found"):Response.ok(orderNodeInstance);
      } catch (Exception var3) {
         log.error("fail to find node instance by id {}, cause:{}", nid, Throwables.getStackTraceAsString(var3));
         return Response.fail("node.instance.query.fail");
      }
   }

   public Response findByIds(List nids) {
      try {
         if(CollectionUtils.isEmpty(nids)) {
            return Response.ok(Collections.emptyList());
         } else {
            List<OrderNodeInstance> nodeInstances = this.orderNodeInstanceDao.findByIds(nids);
            return Response.ok(nodeInstances);
         }
      } catch (Exception var3) {
         log.error("fail to find order instance by ids {}, cause:{}", nids, Throwables.getStackTraceAsString(var3));
         return Response.fail("node.instance.query.fail");
      }
   }
}
