package cn.blmdz.wolf.msg.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.dto.NotificationDto;

public interface NotifyWriteService {
   Response sendNotification(Long var1);

   Response sendNotification(NotificationDto var1);

   Response checkNotification(Long var1, Long var2);

   Response markNotificationChecked(String var1, Long var2);
}
