package io.terminus.parana.web.msg.job;

import com.google.common.base.Objects;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.parana.msg.dto.MessageCriteria;
import io.terminus.parana.msg.model.Channel;
import io.terminus.parana.msg.model.Message;
import io.terminus.parana.msg.model.Message.Status;
import io.terminus.parana.msg.service.MessageReadService;
import io.terminus.parana.msg.service.MessageWriteService;
import io.terminus.parana.web.msg.MsgSender;
import io.terminus.parana.web.msg.job.MsgJobService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class MsgJobServiceImpl implements MsgJobService {
   private static final Logger log = LoggerFactory.getLogger(MsgJobServiceImpl.class);
   private final MessageWriteService messageWriteService;
   private final MessageReadService messageReadService;
   private MsgSender appPushSender;
   private MsgSender emailSender;
   private MsgSender smsSender;
   private MsgSender notifySender;

   @Autowired
   public MsgJobServiceImpl(MessageReadService messageReadService, MessageWriteService messageWriteService) {
      this.messageReadService = messageReadService;
      this.messageWriteService = messageWriteService;
   }

   @Autowired(
      required = false
   )
   @Qualifier("emailSender")
   public void setEmailSender(MsgSender emailSender) {
      this.emailSender = emailSender;
   }

   @Autowired(
      required = false
   )
   @Qualifier("notifySender")
   public void setNotifySender(MsgSender notifySender) {
      this.notifySender = notifySender;
   }

   @Autowired(
      required = false
   )
   @Qualifier("appPushSender")
   public void setAppPushSender(MsgSender appPushSender) {
      this.appPushSender = appPushSender;
   }

   @Autowired(
      required = false
   )
   @Qualifier("smsSender")
   public void setSmsSender(MsgSender smsSender) {
      this.smsSender = smsSender;
   }

   public Long batchSendMessages(Integer batchSize) {
      MessageCriteria criteria = new MessageCriteria();
      criteria.setPageSize(batchSize);
      criteria.setNeedSend(Boolean.valueOf(true));
      Response<Paging<Message>> response = this.messageReadService.pagingMessages(criteria);
      if(!response.isSuccess()) {
         log.error("pagingMessages failed, criteria={}, cause={}", criteria, response.getError());
         throw new JsonResponseException(response.getError());
      } else {
         for(Message message : ((Paging)response.getResult()).getData()) {
            Integer channel = message.getChannel();
            if(Objects.equal(channel, Integer.valueOf(Channel.Notification.value()))) {
               if(this.notifySender == null) {
                  log.error("notifyDispatcher bean missing, message={}", message);
                  throw new JsonResponseException("notify.dispatcher.bean.missing");
               }

               this.notifySender.send(message);
            } else if(Objects.equal(channel, Integer.valueOf(Channel.Email.value()))) {
               if(this.emailSender == null) {
                  log.error("emailDispatcher bean missing, message={}", message);
                  throw new JsonResponseException("email.dispatcher.bean.missing");
               }

               this.emailSender.send(message);
            } else if(Objects.equal(channel, Integer.valueOf(Channel.Sms.value()))) {
               if(this.smsSender == null) {
                  log.error("smsDispatcher bean missing, message={}", message);
                  throw new JsonResponseException("sms.dispatcher.bean.missing");
               }

               this.smsSender.send(message);
            } else if(Objects.equal(channel, Integer.valueOf(Channel.AppPush.value()))) {
               if(this.appPushSender == null) {
                  log.error("appPushDispatcher bean missing, message={}", message);
                  throw new JsonResponseException("app.push.dispatcher.bean.missing");
               }

               this.appPushSender.send(message);
            }
         }

         return ((Paging)response.getResult()).getTotal();
      }
   }

   public Long closeMessages(Integer batchsize) {
      MessageCriteria criteria = new MessageCriteria();
      criteria.setPageSize(batchsize);
      criteria.setNeedClosed(Boolean.valueOf(true));
      Response<Paging<Message>> response = this.messageReadService.pagingMessages(criteria);
      if(!response.isSuccess()) {
         log.error("pagingMessages failed, criteria={}, cause={}", criteria, response.getError());
         throw new JsonResponseException(response.getError());
      } else {
         for(Message message : ((Paging)response.getResult()).getData()) {
            Message toUpdate = new Message();
            toUpdate.setStatus(Integer.valueOf(Status.Closed.value()));
            toUpdate.setId(message.getId());
            Response<Boolean> updateResp = this.messageWriteService.updateMessage(toUpdate);
            if(!updateResp.isSuccess()) {
               log.error("updateMessage failed, message={} cause={}", toUpdate, updateResp.getError());
               throw new JsonResponseException(response.getError());
            }
         }

         return ((Paging)response.getResult()).getTotal();
      }
   }
}
