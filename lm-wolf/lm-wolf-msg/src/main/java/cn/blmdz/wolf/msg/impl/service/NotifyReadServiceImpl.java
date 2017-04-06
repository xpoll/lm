package cn.blmdz.wolf.msg.impl.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Objects;
import com.google.common.base.Optional;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.dto.NotificationCriteria;
import cn.blmdz.wolf.msg.impl.dao.mysql.MessageBoxDao;
import cn.blmdz.wolf.msg.impl.dao.mysql.NotificationDao;
import cn.blmdz.wolf.msg.impl.dao.mysql.ReceiverGroupDao;
import cn.blmdz.wolf.msg.model.Notification;
import cn.blmdz.wolf.msg.model.ReceiverGroup;
import cn.blmdz.wolf.msg.service.NotifyReadService;

@Service
public class NotifyReadServiceImpl implements NotifyReadService {
   private static final Logger log = LoggerFactory.getLogger(NotifyReadServiceImpl.class);
   private final NotificationDao notificationDao;
   private final ReceiverGroupDao receiverGroupDao;
   private final MessageBoxDao messageBoxDao;

   @Autowired
   public NotifyReadServiceImpl(NotificationDao notificationDao, ReceiverGroupDao receiverGroupDao, MessageBoxDao messageBoxDao) {
      this.notificationDao = notificationDao;
      this.receiverGroupDao = receiverGroupDao;
      this.messageBoxDao = messageBoxDao;
   }

   public Response pagingNotifications(NotificationCriteria criteria) {
      Response<Paging<Notification>> response = new Response();

      try {
         Map<String, Object> map = new HashMap();
         map.put("limit", criteria.getLimit());
         map.put("offset", criteria.getOffset());
         if(!Objects.equal(criteria.getShowChecked(), Boolean.TRUE)) {
            map.put("checked", Boolean.valueOf(false));
         }

         if(Objects.equal(criteria.getGroupMessageType(), Integer.valueOf(0))) {
            map.put("audienceId", criteria.getUserId());
         } else {
            ReceiverGroup receiverGroup = this.receiverGroupDao.findByUserId(criteria.getUserId());
            if(receiverGroup == null) {
               log.warn("receiverGroup missing for user={}", criteria.getUserId());
               return Response.ok(new Paging(Long.valueOf(0L), new ArrayList()));
            }

            if(Strings.isNullOrEmpty(receiverGroup.getGroup1()) && Strings.isNullOrEmpty(receiverGroup.getGroup2()) && Strings.isNullOrEmpty(receiverGroup.getGroup3()) && Strings.isNullOrEmpty(receiverGroup.getGroup4())) {
               log.warn("no groups for user={}", criteria.getUserId());
               return Response.ok(new Paging(Long.valueOf(0L), new ArrayList()));
            }

            map.put("group1", receiverGroup.getGroup1());
            map.put("group2", receiverGroup.getGroup2());
            map.put("group3", receiverGroup.getGroup3());
            map.put("group4", receiverGroup.getGroup4());
            map.put("groupMessageType", criteria.getGroupMessageType());
            if(!Objects.equal(criteria.getShowChecked(), Boolean.TRUE)) {
               Set<Long> excludedIds = new HashSet();
               excludedIds.addAll(this.messageBoxDao.getAllCheckedNoticationIds(criteria.getUserId()));
               if(!excludedIds.isEmpty()) {
                  map.put("excludedIds", excludedIds);
               }
            }
         }

         Paging<Notification> page = this.notificationDao.paging(map);
         response.setResult(page);
      } catch (Exception var6) {
         log.error("pagingNotifications failed, criteria={}, \ncause={}", criteria, Throwables.getStackTraceAsString(var6));
         response.setError("paging.notification.failed");
      }

      return response;
   }

   public Response existUncheckedNotification(Long readerId, Integer groupNotificationType) {
      Response<Boolean> response = new Response();

      try {
         if(groupNotificationType != null && groupNotificationType.intValue() != 0) {
            ReceiverGroup receiverGroup = this.receiverGroupDao.findByUserId(readerId);
            if(receiverGroup != null) {
               Map<String, Object> map = new HashMap();
               map.put("group1", receiverGroup.getGroup1());
               map.put("group2", receiverGroup.getGroup2());
               map.put("group3", receiverGroup.getGroup3());
               map.put("group4", receiverGroup.getGroup4());
               map.put("groupMessageType", groupNotificationType);
               Set<Long> excludedIds = new HashSet();
               excludedIds.addAll(this.messageBoxDao.getAllCheckedNoticationIds(readerId));
               if(!excludedIds.isEmpty()) {
                  map.put("excludedIds", excludedIds);
               }

               map.put("checked", Boolean.valueOf(false));
               Long count = this.notificationDao.count(map);
               if(count.longValue() > 0L) {
                  response.setResult(Boolean.TRUE);
               } else {
                  response.setResult(Boolean.FALSE);
               }
            } else {
               response.setResult(Boolean.FALSE);
            }
         } else {
            Optional<Notification> notification = this.notificationDao.getLastUnchecked(readerId);
            if(notification == null) {
               response.setResult(Boolean.FALSE);
            } else {
               response.setResult(Boolean.TRUE);
            }
         }
      } catch (Exception var8) {
         log.error("existUncheckedNotification failed, readerId={}, \ncause={}", readerId, Throwables.getStackTraceAsString(var8));
         response.setError("exist.unchecked.notification.failed");
      }

      return response;
   }
}
