		package cn.blmdz.wolf.web.msg.admin.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.msg.dto.MessageCriteria;
import cn.blmdz.wolf.msg.dto.MessageTemplateCriteria;
import cn.blmdz.wolf.msg.dto.ReceiverGroupCriteria;
import cn.blmdz.wolf.msg.dto.SubscriptionCriteria;
import cn.blmdz.wolf.msg.service.MessageReadService;
import cn.blmdz.wolf.msg.service.MessageTemplateReadService;
import cn.blmdz.wolf.msg.service.ReceiverGroupReadService;
import cn.blmdz.wolf.msg.service.SubscriptionReadService;

@Service
public class AdminMessageService {
   private static final Logger log = LoggerFactory.getLogger(AdminMessageService.class);
   private MessageTemplateReadService messageTemplateReadService;
   private MessageReadService messageReadService;
   private ReceiverGroupReadService receiverGroupReadService;
   private SubscriptionReadService subscriptionReadService;

   @Autowired
   public AdminMessageService(MessageTemplateReadService messageTemplateReadService, MessageReadService messageReadService, ReceiverGroupReadService receiverGroupReadService, SubscriptionReadService subscriptionReadService) {
      this.messageTemplateReadService = messageTemplateReadService;
      this.messageReadService = messageReadService;
      this.receiverGroupReadService = receiverGroupReadService;
      this.subscriptionReadService = subscriptionReadService;
   }

   @Export(
      paramNames = {"criteria"}
   )
   public Response pagingMessageTemplates(MessageTemplateCriteria criteria) {
      return this.messageTemplateReadService.pagingTemplates(criteria);
   }

   @Export(
      paramNames = {"criteria"}
   )
   public Response pagingReceiverGroups(ReceiverGroupCriteria criteria) {
      return this.receiverGroupReadService.pagingReceiverGroups(criteria);
   }

   @Export(
      paramNames = {"criteria"}
   )
   public Response pagingSubscriptions(SubscriptionCriteria criteria) {
      return this.subscriptionReadService.pagingSubscriptions(criteria);
   }

   @Export(
      paramNames = {"criteria"}
   )
   public Response pagingMessages(MessageCriteria criteria) {
      return this.messageReadService.pagingMessages(criteria);
   }
}
