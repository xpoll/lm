package cn.blmdz.wolf.msg.impl.service;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Strings;
import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.wolf.msg.dto.SubscriptionCriteria;
import cn.blmdz.wolf.msg.impl.dao.mysql.SubscriptionDao;
import cn.blmdz.wolf.msg.model.Subscription;
import cn.blmdz.wolf.msg.service.SubscriptionReadService;

@Service
public class SubscriptionReadServiceImpl implements SubscriptionReadService {
   private static final Logger log = LoggerFactory.getLogger(SubscriptionReadServiceImpl.class);
   private final SubscriptionDao subscriptionDao;

   @Autowired
   public SubscriptionReadServiceImpl(SubscriptionDao subscriptionDao) {
      this.subscriptionDao = subscriptionDao;
   }

   public Response findSubscriptionById(Long subscriptionId) {
      Response<Subscription> response = new Response();

      try {
         Subscription subscription = (Subscription)this.subscriptionDao.findById(subscriptionId);
         response.setResult(subscription);
      } catch (Exception var4) {
         log.error("findSubscriptionById failed, id={}, cause={}", subscriptionId, Throwables.getStackTraceAsString(var4));
         response.setError("find.subscription.by.id.failed");
      }

      return response;
   }

   public Response pagingSubscriptions(SubscriptionCriteria criteria) {
      Response<Paging<Subscription>> response = new Response();

      try {
         Paging<Subscription> page = this.subscriptionDao.paging((Map)BeanMapper.map(criteria, Map.class));
         response.setResult(page);
      } catch (Exception var4) {
         log.error("pagingSubscriptions failed, criteria={}, cause={}", criteria, Throwables.getStackTraceAsString(var4));
         response.setError("paging.subscription.failed");
      }

      return response;
   }

   public Response findSubscriptionByAccount(String account) {
      try {
         if(Strings.isNullOrEmpty(account)) {
            log.error("findSubscriptionByAccount failed, account={}, cause={}", account, "account required");
            return Response.fail("account.required");
         } else {
            return Response.ok(this.subscriptionDao.findByAccount(account));
         }
      } catch (Exception var3) {
         log.error("findSubscriptionByAccount failed, account={}, cause={}", account, Throwables.getStackTraceAsString(var3));
         return Response.fail("find.subscription.by.account.failed");
      }
   }

   public Response findSubscriptionByUserId(Long userId) {
      try {
         if(userId == null) {
            log.error("findSubscriptionByUserId failed, userId={}, cause={}", userId, "userId required");
            return Response.fail("user.id.required");
         } else {
            return Response.ok(this.subscriptionDao.findByUserId(userId));
         }
      } catch (Exception var3) {
         log.error("findSubscriptionByUserId failed, userId={}, cause={}", userId, Throwables.getStackTraceAsString(var3));
         return Response.fail("find.subscription.by.user.id.failed");
      }
   }
}
