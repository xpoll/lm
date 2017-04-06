package cn.blmdz.wolf.msg.impl.service;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Strings;
import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.wolf.msg.dto.MessageInfo;
import cn.blmdz.wolf.msg.dto.MessageTemplateCriteria;
import cn.blmdz.wolf.msg.impl.dao.mysql.MessageTemplateDao;
import cn.blmdz.wolf.msg.model.MessageTemplate;
import cn.blmdz.wolf.msg.service.MessageTemplateReadService;

@Service
public class MessageTemplateReadServiceImpl implements MessageTemplateReadService {
   private static final Logger log = LoggerFactory.getLogger(MessageTemplateReadServiceImpl.class);
   private final MessageTemplateDao messageTemplateDao;

   @Autowired
   public MessageTemplateReadServiceImpl(MessageTemplateDao messageTemplateDao) {
      this.messageTemplateDao = messageTemplateDao;
   }

   public Response getMessageTitle(String templateName, Map context) {
      try {
         MessageTemplate template = this.messageTemplateDao.findByName(templateName);
         return template != null && !Strings.isNullOrEmpty(template.getTitle())?Response.ok(template.getTitle()):Response.ok((Object)null);
      } catch (Exception var4) {
         log.error("getMessageTitle failed, templateName={}, context={}, \ncause={}", new Object[]{templateName, context, Throwables.getStackTraceAsString(var4)});
         return Response.fail("get.message.title.failed");
      }
   }

   public Response getMessageContent(String templateName, Map context) {
      try {
         MessageTemplate template = this.messageTemplateDao.findByName(templateName);
         return template != null && !Strings.isNullOrEmpty(template.getContent())?Response.ok(template.getContent()):Response.ok((Object)null);
      } catch (Exception var4) {
         log.error("getMessageContent failed, templateName={}, context={}, \ncause={}", new Object[]{templateName, context, Throwables.getStackTraceAsString(var4)});
         return Response.fail("get.message.content.failed");
      }
   }

   public Response getMessageInfo(String templateName, Map context) {
      try {
         MessageTemplate template = this.messageTemplateDao.findByName(templateName);
         if(template != null && !Strings.isNullOrEmpty(template.getContent())) {
            MessageInfo messageInfo = new MessageInfo();
            messageInfo.setMessageTitle(template.getContent());
            messageInfo.setMessageContent(template.getTitle());
            return Response.ok(messageInfo);
         } else {
            return Response.ok((Object)null);
         }
      } catch (Exception var5) {
         log.error("getMessageContent failed, templateName={}, context={}, \ncause={}", new Object[]{templateName, context, Throwables.getStackTraceAsString(var5)});
         return Response.fail("get.message.content.failed");
      }
   }

   public Response findTemplateById(Long templateId) {
      try {
         MessageTemplate template = (MessageTemplate)this.messageTemplateDao.findById(templateId);
         return Response.ok(template);
      } catch (Exception var3) {
         log.error("findTemplateById failed, id={}, cause={}", templateId, Throwables.getStackTraceAsString(var3));
         return Response.fail("find.template.by.id.failed");
      }
   }

   public Response findTemplateByName(String templateName) {
      try {
         MessageTemplate template = this.messageTemplateDao.findByName(templateName);
         return Response.ok(template);
      } catch (Exception var3) {
         log.error("findTemplateByName failed, templateName={}, cause={}", templateName, Throwables.getStackTraceAsString(var3));
         return Response.fail("find.template.by.name.failed");
      }
   }

   public Response pagingTemplates(MessageTemplateCriteria criteria) {
      try {
         Map<String, Object> map = (Map)BeanMapper.map(criteria, Map.class);
         Paging<MessageTemplate> page = this.messageTemplateDao.paging(map);
         return Response.ok(page);
      } catch (Exception var4) {
         log.error("pagingTemplates failed, criteria={},\ncause={}", criteria, Throwables.getStackTraceAsString(var4));
         return Response.fail("paging.templates.failed");
      }
   }
}
