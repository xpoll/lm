package cn.blmdz.wolf.web.msg.impl.notify.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.msg.dto.NotificationCriteria;
import cn.blmdz.wolf.msg.model.Notification;
import cn.blmdz.wolf.msg.service.NotifyReadService;
import cn.blmdz.wolf.msg.service.NotifyWriteService;

@Controller
@RequestMapping({"/api/msg/notify"})
public class Notifications {
   private static final Logger log = LoggerFactory.getLogger(Notifications.class);
   private final NotifyWriteService notifyWriteService;
   private final NotifyReadService notifyReadService;

   @Autowired
   public Notifications(NotifyReadService notifyReadService, NotifyWriteService notifyWriteService) {
      this.notifyReadService = notifyReadService;
      this.notifyWriteService = notifyWriteService;
   }

   @RequestMapping(
      value = {"/check"},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   @ResponseBody
   public Notification checkNotification(Long notificationId) {
      BaseUser user = UserUtil.getCurrentUser();
      Response<Notification> response = this.notifyWriteService.checkNotification(notificationId, user.getId());
      if(response.isSuccess()) {
         return (Notification)response.getResult();
      } else {
         log.error("checkNotification failed, notificationId={}, user={}, cause={}", new Object[]{notificationId, user, response.getError()});
         throw new JsonResponseException(response.getError());
      }
   }

   @RequestMapping(
      value = {"/markChecked"},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   @ResponseBody
   public Boolean markChecked(String ids) {
      BaseUser user = UserUtil.getCurrentUser();
      Response<Boolean> response = this.notifyWriteService.markNotificationChecked(ids, user.getId());
      if(response.isSuccess()) {
         return (Boolean)response.getResult();
      } else {
         log.error("markNotificationChecked failed, ids={}, user={}, cause={}", new Object[]{ids, user, response.getError()});
         throw new JsonResponseException(response.getError());
      }
   }

   @RequestMapping(
      value = {"/existUnchecked"},
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   @ResponseBody
   public Boolean existUnchecked(Integer groupNotificationType) {
      BaseUser user = UserUtil.getCurrentUser();
      Response<Boolean> response = this.notifyReadService.existUncheckedNotification(user.getId(), groupNotificationType);
      if(response.isSuccess()) {
         return (Boolean)response.getResult();
      } else {
         log.error("existUnchecked failed, user={}, groupNotificationType={}, cause={}", new Object[]{user, groupNotificationType, response.getError()});
         throw new JsonResponseException(response.getError());
      }
   }

   @RequestMapping(
      value = {"/paging"},
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   @ResponseBody
   public Paging pagingNotifications(NotificationCriteria criteria) {
      BaseUser user = UserUtil.getCurrentUser();
      criteria.setUserId(user.getId());
      Response<Paging<Notification>> response = this.notifyReadService.pagingNotifications(criteria);
      if(response.isSuccess()) {
         return (Paging)response.getResult();
      } else {
         log.error("pagingNotifications failed, criteria={}, cause={}", criteria, response.getError());
         throw new JsonResponseException(response.getError());
      }
   }

   @Export(
      paramNames = {"criteria"}
   )
   public Response pagingNotificationsService(NotificationCriteria criteria) {
      return this.notifyReadService.pagingNotifications(criteria);
   }
}
