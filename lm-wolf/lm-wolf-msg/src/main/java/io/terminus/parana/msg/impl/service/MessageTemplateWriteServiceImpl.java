package io.terminus.parana.msg.impl.service;

import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.msg.impl.dao.mysql.MessageTemplateDao;
import io.terminus.parana.msg.model.MessageTemplate;
import io.terminus.parana.msg.service.MessageTemplateWriteService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;

@Service
public class MessageTemplateWriteServiceImpl implements MessageTemplateWriteService {
   private static final Logger log = LoggerFactory.getLogger(MessageTemplateWriteServiceImpl.class);
   private final MessageTemplateDao messageTemplateDao;

   @Autowired
   public MessageTemplateWriteServiceImpl(MessageTemplateDao messageTemplateDao) {
      this.messageTemplateDao = messageTemplateDao;
   }

   public Response createTemplate(MessageTemplate messageTemplate) {
      Response<Long> response = new Response();

      try {
         if(Strings.isNullOrEmpty(messageTemplate.getName())) {
            log.error("createTemplate failed, template={}, \ncause={}", messageTemplate, "message template name required");
            response.setError("message.template.name.required");
            return response;
         }

         this.messageTemplateDao.create(messageTemplate);
         response.setResult(messageTemplate.getId());
      } catch (DuplicateKeyException var4) {
         log.error("createTemplate failed, template={}, \ncause={}", messageTemplate, Throwables.getStackTraceAsString(var4));
         response.setError("create.template.failed.by.duplicate.key");
      } catch (Exception var5) {
         log.error("createTemplate failed, template={}, \ncause={}", messageTemplate, Throwables.getStackTraceAsString(var5));
         response.setError("create.template.failed");
      }

      return response;
   }

   public Response updateTemplate(MessageTemplate messageTemplate) {
      try {
         Boolean result = this.messageTemplateDao.update(messageTemplate);
         return Response.ok(result);
      } catch (Exception var3) {
         log.error("updateTemplate failed, template={}, \ncause={}", messageTemplate, Throwables.getStackTraceAsString(var3));
         return Response.fail("update.template.failed");
      }
   }

   public Response deleteTemplateById(Long templateId) {
      try {
         Boolean result = this.messageTemplateDao.delete(templateId);
         return Response.ok(result);
      } catch (Exception var3) {
         log.error("deleteTemplateById failed, id={}, cause={}", templateId, Throwables.getStackTraceAsString(var3));
         return Response.fail("delete.template.by.id.failed");
      }
   }
}
