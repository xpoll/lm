package io.terminus.parana.msg.service;

import io.terminus.common.model.Response;
import io.terminus.parana.msg.dto.NotificationDto;

public interface NotifyWriteService {
   Response sendNotification(Long var1);

   Response sendNotification(NotificationDto var1);

   Response checkNotification(Long var1, Long var2);

   Response markNotificationChecked(String var1, Long var2);
}
