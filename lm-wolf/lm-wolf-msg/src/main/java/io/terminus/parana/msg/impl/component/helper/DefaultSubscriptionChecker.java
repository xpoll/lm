package io.terminus.parana.msg.impl.component.helper;

import io.terminus.common.utils.JsonMapper;
import io.terminus.parana.msg.component.SubscriptionChecker;
import io.terminus.parana.msg.impl.dao.mysql.SubscriptionDao;
import io.terminus.parana.msg.model.Subscription;
import java.util.ArrayList;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;

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
