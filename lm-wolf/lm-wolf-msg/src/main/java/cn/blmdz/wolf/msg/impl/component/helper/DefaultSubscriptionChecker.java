package cn.blmdz.wolf.msg.impl.component.helper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.msg.component.SubscriptionChecker;
import cn.blmdz.wolf.msg.impl.dao.mysql.SubscriptionDao;
import cn.blmdz.wolf.msg.model.Subscription;

public class DefaultSubscriptionChecker implements SubscriptionChecker {
   private final SubscriptionDao subscriptionDao;

   @Autowired
   public DefaultSubscriptionChecker(SubscriptionDao subscriptionDao) {
      this.subscriptionDao = subscriptionDao;
   }

   public List checkSubscription(String receivers) {
      List<String> receiverList = (List)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(receivers, List.class);
      List<String> actualReceivers = new ArrayList();

      for(String receiver : receiverList) {
         Subscription subscription = this.subscriptionDao.findByAccount(receiver);
         if(subscription != null) {
            actualReceivers.add(receiver);
         }
      }

      return actualReceivers;
   }
}
