package io.terminus.parana.web.msg.admin.service;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;
import io.terminus.parana.msg.dto.MessageCriteria;
import io.terminus.parana.msg.dto.MessageTemplateCriteria;
import io.terminus.parana.msg.dto.ReceiverGroupCriteria;
import io.terminus.parana.msg.dto.SubscriptionCriteria;
import io.terminus.parana.msg.service.MessageReadService;
import io.terminus.parana.msg.service.MessageTemplateReadService;
import io.terminus.parana.msg.service.ReceiverGroupReadService;
import io.terminus.parana.msg.service.SubscriptionReadService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

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
