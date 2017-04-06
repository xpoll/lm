package cn.blmdz.wolf.msg.impl.service;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Preconditions;
import com.google.common.base.Throwables;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.msg.dto.NotificationDto;
import cn.blmdz.wolf.msg.impl.dao.mysql.MessageBoxDao;
import cn.blmdz.wolf.msg.impl.dao.mysql.MessageDao;
import cn.blmdz.wolf.msg.impl.dao.mysql.NotificationDao;
import cn.blmdz.wolf.msg.impl.dao.mysql.ReceiverGroupDao;
import cn.blmdz.wolf.msg.model.Message;
import cn.blmdz.wolf.msg.model.Message.GroupMessageType;
import cn.blmdz.wolf.msg.model.MessageBox;
import cn.blmdz.wolf.msg.model.Notification;
import cn.blmdz.wolf.msg.model.ReceiverGroup;
import cn.blmdz.wolf.msg.service.NotifyWriteService;

@Service
public class NotifyWriteServiceImpl implements NotifyWriteService {
   private static final Logger log = LoggerFactory.getLogger(NotifyWriteServiceImpl.class);
   private final NotificationDao notificationDao;
   private final MessageDao messageDao;
   private final ReceiverGroupDao receiverGroupDao;
   private final MessageBoxDao messageBoxDao;

   @Autowired
   public NotifyWriteServiceImpl(NotificationDao notificationDao, MessageDao messageDao, ReceiverGroupDao receiverGroupDao, MessageBoxDao messageBoxDao) {
      this.notificationDao = notificationDao;
      this.messageDao = messageDao;
      this.receiverGroupDao = receiverGroupDao;
      this.messageBoxDao = messageBoxDao;
   }

   public Response sendNotification(Long messageId) {
      Response<String> response = new Response();

      try {
         Message message = (Message)this.messageDao.findById(messageId);
         if(message == null) {
            log.error("findMessageById failed, id={}", messageId);
            throw new ServiceException("find.message.by.id.failed");
         }

         List<Long> notificationIds = new ArrayList();
         if(!message.getGroupMessageType().equals(Integer.valueOf(GroupMessageType.NotGroupMessage.value()))) {
            Map<String, String> groups = (Map)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(message.getReceivers(), Map.class);
            Notification notification = new Notification();
            notification.setChecked(Boolean.valueOf(false));
            notification.setSubject(message.getTitle());
            notification.setContent(message.getContent());
            notification.setAudienceGroup1((String)groups.get("group1"));
            notification.setAudienceGroup2((String)groups.get("group2"));
            notification.setAudienceGroup3((String)groups.get("group3"));
            notification.setAudienceGroup4((String)groups.get("group4"));
            this.notificationDao.create(notification);
            notificationIds.add(notification.getId());
         } else {
            List<String> receivers = (List)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(message.getReceivers(), List.class);
            List<Notification> notificationList = new ArrayList();

            for(String receiver : receivers) {
               Notification notification = new Notification();
               notification.setChecked(Boolean.valueOf(false));
               notification.setAudienceId(Long.valueOf(Long.parseLong(receiver)));
               notification.setSubject(message.getTitle());
               notification.setContent(message.getContent());
               notificationList.add(notification);
            }

            for(Notification notification : notificationList) {
               this.notificationDao.create(notification);
               notificationIds.add(notification.getId());
            }
         }

         response.setResult(JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(notificationIds));
      } catch (ServiceException var10) {
         log.error("sendNotification failed, messageId={}, cause={}", messageId, var10.getMessage());
         response.setError(var10.getMessage());
      } catch (Exception var11) {
         log.error("sendNotification failed, messageId={}, cause={}", messageId, Throwables.getStackTraceAsString(var11));
         response.setError("send.notification.failed");
      }

      return response;
   }

   public Response sendNotification(NotificationDto notification) {
      Response<String> response = new Response();

      try {
         List<Long> notificationIds = new ArrayList();
         if(notification.getAudienceIds() != null) {
            for(Long userId : notification.getAudienceIds()) {
               Notification notificationToCreate = (Notification)BeanMapper.map(notification, Notification.class);
               notificationToCreate.setChecked(Boolean.valueOf(false));
               notificationToCreate.setAudienceId(userId);
               this.notificationDao.create(notificationToCreate);
               notificationIds.add(notificationToCreate.getId());
            }
         } else {
            Notification notificationToCreate = (Notification)BeanMapper.map(notification, Notification.class);
            notificationToCreate.setChecked(Boolean.valueOf(false));
            this.notificationDao.create(notificationToCreate);
            notificationIds.add(notificationToCreate.getId());
         }

         response.setResult(JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(notificationIds));
      } catch (ServiceException var7) {
         log.error("sendNotification failed, notification={}, cause={}", notification, var7.getMessage());
         response.setError(var7.getMessage());
      } catch (Exception var8) {
         log.error("sendNotification failed, notification={}, cause={}", notification, Throwables.getStackTraceAsString(var8));
         response.setError("send.notification.failed");
      }

      return response;
   }

   public Response checkNotification(Long id, Long readerId) {
      Response<Notification> response = new Response();

      try {
         Notification notification = (Notification)this.notificationDao.findById(id);
         if(notification == null) {
            log.error("findNotificationById failed, notificationId={}, readerId={}", id, readerId);
            throw new ServiceException("find.notification.byid.failed");
         }

         if(notification.getAudienceId() != null) {
            if(!notification.getAudienceId().equals(readerId)) {
               log.error("check notification permission deny, readerId={}, notification={}", readerId, notification);
               throw new ServiceException("check.notification.permission.deny");
            }

            Notification toUpdate = new Notification();
            toUpdate.setId(id);
            toUpdate.setChecked(Boolean.valueOf(true));
            this.notificationDao.update(toUpdate);
         } else {
            ReceiverGroup receiverGroup = this.receiverGroupDao.findByUserId(readerId);
            if(receiverGroup == null) {
               log.error("findReceiverGroupByUserId failed, userId={}", readerId);
               throw new ServiceException("find.receiver.group.by.user.id.failed");
            }

            Set<Integer> msgBoxes = new HashSet();
            if(notification.getAudienceGroup1() != null && notification.getAudienceGroup1().equals(receiverGroup.getGroup1())) {
               msgBoxes.add(Integer.valueOf(1));
            }

            if(notification.getAudienceGroup2() != null && notification.getAudienceGroup2().equals(receiverGroup.getGroup2())) {
               msgBoxes.add(Integer.valueOf(2));
            }

            if(notification.getAudienceGroup3() != null && notification.getAudienceGroup3().equals(receiverGroup.getGroup3())) {
               msgBoxes.add(Integer.valueOf(3));
            }

            if(notification.getAudienceGroup4() != null && notification.getAudienceGroup4().equals(receiverGroup.getGroup4())) {
               msgBoxes.add(Integer.valueOf(4));
            }

            if(msgBoxes.isEmpty()) {
               log.error("check notification permission deny, readerId={}, group={}, notification={}", new Object[]{readerId, receiverGroup, notification});
               throw new ServiceException("check.notification.permission.deny");
            }

            for(Integer box : msgBoxes) {
               MessageBox messageBox = new MessageBox();
               messageBox.setUserId(readerId);
               messageBox.setBoxIndex(box);
               messageBox.setNotificationId(id);
               this.messageBoxDao.create(messageBox);
            }
         }

         response.setResult(notification);
      } catch (ServiceException var10) {
         log.error("checkNotification failed, notificationId={}, readerId={}, cause={}", new Object[]{id, readerId, var10.getMessage()});
         response.setError(var10.getMessage());
      } catch (Exception var11) {
         log.error("checkNotification failed, notificationId={}, readerId={}, cause={}", new Object[]{id, readerId, Throwables.getStackTraceAsString(var11)});
         response.setError("check.notification.failed");
      }

      return response;
   }

   public Response markNotificationChecked(String ids, Long readerId) {
      try {
         List<Long> idList = (List)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(ids, JsonMapper.JSON_NON_EMPTY_MAPPER.createCollectionType(List.class, new Class[]{Long.class}));
         Preconditions.checkNotNull(idList);

         for(Long id : idList) {
            Response<Notification> notificationResponse = this.checkNotification(id, readerId);
            if(!notificationResponse.isSuccess()) {
               return Response.fail(notificationResponse.getError());
            }
         }

         return Response.ok(Boolean.TRUE);
      } catch (Exception var7) {
         log.error("markNotificationChecked failed, ids={}, readerId={}, cause={}", new Object[]{ids, readerId, Throwables.getStackTraceAsString(var7)});
         return Response.fail("mark.notification.checked.failed");
      }
   }
}
