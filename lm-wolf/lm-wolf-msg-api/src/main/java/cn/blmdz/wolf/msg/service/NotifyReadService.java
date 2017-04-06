package cn.blmdz.wolf.msg.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.dto.NotificationCriteria;

public interface NotifyReadService {
   Response pagingNotifications(NotificationCriteria var1);

   Response existUncheckedNotification(Long var1, Integer var2);
}
