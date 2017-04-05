package io.terminus.parana.web.msg.admin.controller;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.parana.msg.dto.SubscriptionCriteria;
import io.terminus.parana.msg.model.Subscription;
import io.terminus.parana.msg.service.SubscriptionReadService;
import io.terminus.parana.msg.service.SubscriptionWriteService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping({"/api/msg/subscription"})
public class Subscriptions {
   private static final Logger log = LoggerFactory.getLogger(Subscriptions.class);
   private final SubscriptionReadService subscriptionReadService;
   private final SubscriptionWriteService subscriptionWriteService;

   @Autowired
   public Subscriptions(SubscriptionReadService subscriptionReadService, SubscriptionWriteService subscriptionWriteService) {
      this.subscriptionReadService = subscriptionReadService;
      this.subscriptionWriteService = subscriptionWriteService;
   }

   @RequestMapping(
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   @ResponseBody
   public Long createSubscription(@RequestBody Subscription subscription) {
      Response<Long> response = this.subscriptionWriteService.createSubscription(subscription);
      if(response.isSuccess()) {
         return (Long)response.getResult();
      } else {
         log.error("createSubscription failed, subscription={}, cause={}", subscription, response.getError());
         throw new JsonResponseException(response.getError());
      }
   }

   @RequestMapping(
      method = {RequestMethod.PUT},
      produces = {"application/json"}
   )
   @ResponseBody
   public Boolean updateSubcription(@RequestBody Subscription subscription) {
      Response<Boolean> response = this.subscriptionWriteService.updateSubscription(subscription);
      if(response.isSuccess()) {
         return (Boolean)response.getResult();
      } else {
         log.error("updateSubcription failed, subscription={}, cause={}", subscription, response.getError());
         throw new JsonResponseException(response.getError());
      }
   }

   @RequestMapping(
      value = {"/{subscriptionId}"},
      method = {RequestMethod.DELETE},
      produces = {"application/json"}
   )
   @ResponseBody
   public Boolean deleteSubscription(@PathVariable("subscriptionId") Long subscriptionId) {
      Response<Boolean> response = this.subscriptionWriteService.deleteSubscriptionById(subscriptionId);
      if(response.isSuccess()) {
         return (Boolean)response.getResult();
      } else {
         log.error("deleteSubscription failed, subscriptionId={}, cause={}", subscriptionId, response.getError());
         throw new JsonResponseException(response.getError());
      }
   }

   @RequestMapping(
      value = {"/paging"},
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   @ResponseBody
   public Paging pagingSubscriptions(SubscriptionCriteria criteria) {
      Response<Paging<Subscription>> response = this.subscriptionReadService.pagingSubscriptions(criteria);
      if(response.isSuccess()) {
         return (Paging)response.getResult();
      } else {
         log.error("pagingSubscriptions failed, criteria={}, cause={}", criteria, response.getError());
         throw new JsonResponseException(response.getError());
      }
   }
}
