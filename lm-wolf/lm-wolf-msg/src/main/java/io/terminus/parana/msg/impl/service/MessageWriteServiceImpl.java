package io.terminus.parana.msg.impl.service;

import com.google.common.base.Preconditions;
import com.google.common.base.Throwables;
import com.google.common.collect.ImmutableMap;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.JsonMapper;
import io.terminus.parana.msg.component.MessagePostHandler;
import io.terminus.parana.msg.component.MessagePreHandler;
import io.terminus.parana.msg.component.MessageValidatorChain;
import io.terminus.parana.msg.impl.dao.mysql.MessageDao;
import io.terminus.parana.msg.impl.dao.mysql.NotificationDao;
import io.terminus.parana.msg.impl.dao.mysql.ReceiverGroupDao;
import io.terminus.parana.msg.impl.dao.mysql.SubscriptionDao;
import io.terminus.parana.msg.model.Channel;
import io.terminus.parana.msg.model.Message;
import io.terminus.parana.msg.model.Notification;
import io.terminus.parana.msg.model.Message.Status;
import io.terminus.parana.msg.service.MessageWriteService;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class MessageWriteServiceImpl implements MessageWriteService {
   private static final Logger log = LoggerFactory.getLogger(MessageWriteServiceImpl.class);
   private final MessageDao messageDao;
   private final NotificationDao notificationDao;
   private final ReceiverGroupDao receiverGroupDao;
   private final SubscriptionDao subscriptionDao;
   private MessageValidatorChain messageValidatorChain;
   private MessagePreHandler messagePreHandler;
   private MessagePostHandler messagePostHandler;

   @Autowired
   public MessageWriteServiceImpl(MessageDao messageDao, ReceiverGroupDao receiverGroupDao, SubscriptionDao subscriptionDao, NotificationDao notificationDao) {
      this.messageDao = messageDao;
      this.receiverGroupDao = receiverGroupDao;
      this.subscriptionDao = subscriptionDao;
      this.notificationDao = notificationDao;
   }

   @Autowired(
      required = false
   )
   public void setMessageValidatorChain(MessageValidatorChain messageValidatorChain) {
      this.messageValidatorChain = messageValidatorChain;
   }

   @Autowired(
      required = false
   )
   public void setMessagePreHandler(MessagePreHandler messagePreHandler) {
      this.messagePreHandler = messagePreHandler;
   }

   @Autowired(
      required = false
   )
   public void setMessagePostHandler(MessagePostHandler messagePostHandler) {
      this.messagePostHandler = messagePostHandler;
   }

   public Response sendMessage(Integer channel, String title, String content, String toes, String extra) {
      try {
         Message message = (Message)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(extra, Message.class);
         if(message == null) {
            message = new Message();
         }

         message.setChannel(channel);
         message.setTitle(title);
         message.setContent(content);
         message.setReceivers(toes);
         return this.createMessage(message);
      } catch (Exception var7) {
         log.error("sendMessage failed, params={}, \ncause={}", ImmutableMap.of("channel", channel, "content", content, "toes", toes, "extra", extra), Throwables.getStackTraceAsString(var7));
         return Response.fail("send.message.failed");
      }
   }

   public Response sendMessageViaTemplate(Integer channel, String template, Map context, String toes, String extra) {
      try {
         Message message = (Message)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(extra, Message.class);
         if(message == null) {
            message = new Message();
         }

         message.setChannel(channel);
         message.setTemplate(template);
         message.setData(JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(context));
         message.setReceivers(toes);
         return this.createMessage(message);
      } catch (Exception var7) {
         log.error("sendMessage failed, params={}, \ncause={}", ImmutableMap.of("channel", channel, "template", template, "context", context, "toes", toes, "extra", extra), Throwables.getStackTraceAsString(var7));
         return Response.fail("send.message.failed");
      }
   }

   public Response createMessage(Message message) {
      Response<Long> response = new Response();

      try {
         if(this.messagePreHandler != null) {
            this.messagePreHandler.preHandle(message);
         }

         if(this.messageValidatorChain != null) {
            this.messageValidatorChain.check(message);
         }

         if(this.messagePostHandler != null) {
            this.messagePostHandler.postHandle(message);
         }

         this.messageDao.create(message);
         response.setResult(message.getId());
      } catch (ServiceException var4) {
         log.error("createMessage failed, message={}, \ncause={}", message, var4.getMessage());
         response.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("createMessage failed, message={}, \ncause={}", message, Throwables.getStackTraceAsString(var5));
         response.setError("create.message.failed");
      }

      return response;
   }

   public Response updateMessage(Message message) {
      try {
         if(message.getStatus() != null) {
            Message origin = (Message)this.messageDao.findById(message.getId());
            this.updateMessageStatus(origin, origin.getStatus(), message.getStatus());
         }

         Boolean result = this.messageDao.update(message);
         return Response.ok(result);
      } catch (Exception var3) {
         log.error("updateMessage failed, params={}, \ncause={}", message, Throwables.getStackTraceAsString(var3));
         return Response.fail("update.message.failed");
      }
   }

   protected void updateMessageStatus(Message origin, Integer statusFrom, Integer statusTo) {
      if(!Status.canChange(statusFrom, statusTo).booleanValue()) {
         log.error("updateMessageStatus failed, message={}, statusFrom={}, statusTo={}", new Object[]{origin, statusFrom, statusTo});
         throw new ServiceException("illegal.message.status.change");
      } else {
         if(origin.getChannel().equals(Integer.valueOf(Channel.Notification.value())) && statusFrom.equals(Integer.valueOf(Status.SendSuccess.value())) && statusTo.equals(Integer.valueOf(Status.Closed.value()))) {
            Preconditions.checkNotNull(origin.getChannelOutput());
            List<Long> notificationIds = (List)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(origin.getChannelOutput(), JsonMapper.JSON_NON_EMPTY_MAPPER.createCollectionType(List.class, new Class[]{Long.class}));
            Preconditions.checkNotNull(notificationIds);

            for(Long id : notificationIds) {
               Notification toUpdate = new Notification();
               toUpdate.setId(id);
               toUpdate.setChecked(Boolean.valueOf(true));
               this.notificationDao.update(toUpdate);
            }
         }

      }
   }
}
