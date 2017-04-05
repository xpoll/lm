package io.terminus.parana.msg.service;

import io.terminus.common.model.Response;
import io.terminus.parana.msg.dto.NotificationCriteria;

public interface NotifyReadService {
   Response pagingNotifications(NotificationCriteria var1);

   Response existUncheckedNotification(Long var1, Integer var2);
}
