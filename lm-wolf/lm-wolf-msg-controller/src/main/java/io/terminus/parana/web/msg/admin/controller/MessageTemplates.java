package io.terminus.parana.web.msg.admin.controller;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.BaseUser;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.pampas.common.UserUtil;
import io.terminus.parana.msg.dto.MessageTemplateCriteria;
import io.terminus.parana.msg.model.MessageTemplate;
import io.terminus.parana.msg.service.MessageTemplateReadService;
import io.terminus.parana.msg.service.MessageTemplateWriteService;
import java.util.Date;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping({"/api/msg/template"})
public class MessageTemplates {
   private static final Logger log = LoggerFactory.getLogger(MessageTemplates.class);
   private final MessageTemplateWriteService messageTemplateWriteService;
   private final MessageTemplateReadService messageTemplateReadService;

   @Autowired
   public MessageTemplates(MessageTemplateReadService messageTemplateReadService, MessageTemplateWriteService messageTemplateWriteService) {
      this.messageTemplateReadService = messageTemplateReadService;
      this.messageTemplateWriteService = messageTemplateWriteService;
   }

   @RequestMapping(
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   @ResponseBody
   public Long createMessageTemplate(@RequestBody MessageTemplate messageTemplate) {
      BaseUser user = UserUtil.getCurrentUser();
      messageTemplate.setCreatorId(user.getId());
      messageTemplate.setCreatorName(user.getName());
      messageTemplate.setCreatedAt(new Date());
      Response<Long> response = this.messageTemplateWriteService.createTemplate(messageTemplate);
      if(response.isSuccess()) {
         return (Long)response.getResult();
      } else {
         log.error("createMessageTemplate failed, cause={}, template={}", response.getError(), messageTemplate);
         throw new JsonResponseException(response.getError());
      }
   }

   @RequestMapping(
      method = {RequestMethod.PUT},
      produces = {"application/json"}
   )
   @ResponseBody
   public Boolean updateMessageTemplate(@RequestBody MessageTemplate messageTemplate) {
      BaseUser user = UserUtil.getCurrentUser();
      messageTemplate.setCreatorId(user.getId());
      messageTemplate.setCreatorName(user.getName());
      messageTemplate.setCreatedAt(new Date());
      Response<Boolean> response = this.messageTemplateWriteService.updateTemplate(messageTemplate);
      if(response.isSuccess()) {
         return (Boolean)response.getResult();
      } else {
         log.error("updateMessageTemplate failed, cause={}, template={}", response.getError(), messageTemplate);
         throw new JsonResponseException(response.getError());
      }
   }

   @RequestMapping(
      value = {"/{templateId}"},
      method = {RequestMethod.DELETE},
      produces = {"application/json"}
   )
   @ResponseBody
   public Boolean deleteMessageTemplate(@PathVariable("templateId") Long templateId) {
      Response<Boolean> response = this.messageTemplateWriteService.deleteTemplateById(templateId);
      if(response.isSuccess()) {
         return (Boolean)response.getResult();
      } else {
         log.error("deleteMessageTemplate failed, template={}, cause={}", templateId, response.getError());
         throw new JsonResponseException(response.getError());
      }
   }

   @RequestMapping(
      value = {"/paging"},
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   @ResponseBody
   public Paging pagingMessageTemplates(MessageTemplateCriteria criteria) {
      Response<Paging<MessageTemplate>> response = this.messageTemplateReadService.pagingTemplates(criteria);
      if(response.isSuccess()) {
         return (Paging)response.getResult();
      } else {
         log.error("pagingMessageTemplate failed, criteria={}, cause={}", criteria, response.getError());
         throw new JsonResponseException(response.getError());
      }
   }
}
