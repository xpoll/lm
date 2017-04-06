package cn.blmdz.wolf.msg.impl.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.impl.dao.mysql.SubscriptionDao;
import cn.blmdz.wolf.msg.model.Subscription;
import cn.blmdz.wolf.msg.service.SubscriptionWriteService;

@Service
public class SubscriptionWriteServiceImpl implements SubscriptionWriteService {
   private static final Logger log = LoggerFactory.getLogger(SubscriptionWriteServiceImpl.class);
   private final SubscriptionDao subscriptionDao;

   @Autowired
   public SubscriptionWriteServiceImpl(SubscriptionDao subscriptionDao) {
      this.subscriptionDao = subscriptionDao;
   }

   public Response createSubscription(Subscription subscription) {
      Response<Long> response = new Response();

      try {
         this.subscriptionDao.create(subscription);
         response.setResult(subscription.getId());
      } catch (Exception var4) {
         log.error("createSubscription failed, params={}, cause={}", subscription, Throwables.getStackTraceAsString(var4));
         response.setError("create.subscription.failed");
      }

      return response;
   }

   public Response updateSubscription(Subscription subscription) {
      Response<Boolean> response = new Response();

      try {
         Boolean result = this.subscriptionDao.update(subscription);
         response.setResult(result);
      } catch (Exception var4) {
         log.error("updateSubscription failed, params={}, cause={}", subscription, Throwables.getStackTraceAsString(var4));
         response.setError("update.subscription.failed");
      }

      return response;
   }

   public Response deleteSubscriptionById(Long subscriptionId) {
      try {
         return Response.ok(this.subscriptionDao.delete(subscriptionId));
      } catch (Exception var3) {
         log.error("deleteSubscriptionById failed, subscriptionId={}, cause={}", subscriptionId, Throwables.getStackTraceAsString(var3));
         return Response.fail("delete.subscription.by.id.failed");
      }
   }
}
