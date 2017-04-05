package io.terminus.parana.msg.impl.service;

import com.google.common.base.Objects;
import com.google.common.base.Throwables;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.common.utils.BeanMapper;
import io.terminus.common.utils.JsonMapper;
import io.terminus.parana.msg.component.SubscriptionChecker;
import io.terminus.parana.msg.dto.MessageCriteria;
import io.terminus.parana.msg.impl.dao.mysql.MessageDao;
import io.terminus.parana.msg.model.Message;
import io.terminus.parana.msg.service.MessageReadService;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class MessageReadServiceImpl implements MessageReadService {
   private static final Logger log = LoggerFactory.getLogger(MessageReadServiceImpl.class);
   private final MessageDao messageDao;
   private SubscriptionChecker subscriptionChecker;

   @Autowired
   public MessageReadServiceImpl(MessageDao messageDao) {
      this.messageDao = messageDao;
   }

   @Autowired(
      required = false
   )
   public void setSubscriptionChecker(SubscriptionChecker subscriptionChecker) {
      this.subscriptionChecker = subscriptionChecker;
   }

   public Response pagingMessages(MessageCriteria criteria) {
      try {
         Paging<Message> page = this.messageDao.paging((Map)BeanMapper.map(criteria, Map.class));
         return Response.ok(page);
      } catch (Exception var3) {
         log.error("pagingMessages failed, criteria={}, \ncause={}", criteria, Throwables.getStackTraceAsString(var3));
         return Response.fail("paging.messages.failed");
      }
   }

   public Response findMessageById(Long messageId) {
      try {
         Message message = (Message)this.messageDao.findById(messageId);
         return Response.ok(message);
      } catch (Exception var3) {
         log.error("findMessageById failed, id={}, cause={}", messageId, Throwables.getStackTraceAsString(var3));
         return Response.fail("find.message.by.id.failed");
      }
   }

   public Response getMessageStatus(Long messageId) {
      Response<Integer> response = new Response();

      try {
         Message message = (Message)this.messageDao.findById(messageId);
         if(message == null) {
            log.error("getMessageStatus failed, id={}, cause={}", messageId, "message not exists.");
            response.setError("message.not.exists");
         } else {
            response.setResult(message.getStatus());
         }
      } catch (Exception var4) {
         log.error("getMessageStatus failed, id={}, cause={}", messageId, Throwables.getStackTraceAsString(var4));
         response.setError("get.message.status.failed");
      }

      return response;
   }

   public Response getMessageActualReceivers(Long messageId) {
      Response<String> response = new Response();

      try {
         Message message = (Message)this.messageDao.findById(messageId);
         if(message == null) {
            throw new ServiceException("find.message.by.id.failed");
         }

         if(this.subscriptionChecker != null && Objects.equal(message.getCheckSubscribe(), Boolean.TRUE)) {
            log.info("original receivers={}", message.getReceivers());
            List<String> actualReceivers = this.subscriptionChecker.checkSubscription(message.getReceivers());
            log.info("actual receivers={}", actualReceivers);
            response.setResult(JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(actualReceivers));
         }

         if(response.getResult() == null) {
            response.setResult(message.getReceivers());
         }
      } catch (ServiceException var5) {
         log.error("getMessageActualReceivers failed, messageId={}, \ncause={}", messageId, var5.getMessage());
         response.setError(var5.getMessage());
      } catch (Exception var6) {
         log.error("getMessageActualReceivers failed, messageId={}, \ncause={}", messageId, Throwables.getStackTraceAsString(var6));
         response.setError("create.message.failed");
      }

      return response;
   }

   public Response getMessageActualReceivers(String originReceivers) {
      try {
         if(this.subscriptionChecker != null) {
            log.info("original receivers={}", originReceivers);
            List<String> actualReceiversList = this.subscriptionChecker.checkSubscription(originReceivers);
            log.info("actual receivers={}", actualReceiversList);
            return Response.ok(JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(actualReceiversList));
         }
      } catch (Exception var3) {
         log.error("getMessageActualReceivers failed, cause={}", Throwables.getStackTraceAsString(var3));
      }

      return Response.ok(originReceivers);
   }
}
