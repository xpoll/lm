package cn.blmdz.wolf.web.msg.impl.common.db;

import com.github.jknack.handlebars.Template;

import cn.blmdz.wolf.common.util.RespUtil;
import cn.blmdz.wolf.msg.model.MessageTemplate;
import cn.blmdz.wolf.msg.service.MessageTemplateReadService;
import cn.blmdz.wolf.web.msg.MsgGatewayBuilder;
import cn.blmdz.wolf.web.msg.MsgTemplateApplication;
import cn.blmdz.wolf.web.msg.impl.common.MsgTemplateApplicationBase;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DbMsgTemplateApplication extends MsgTemplateApplicationBase implements MsgTemplateApplication {
   private static final Logger log = LoggerFactory.getLogger(DbMsgTemplateApplication.class);
   protected final MessageTemplateReadService messageTemplateReadService;
   protected final MsgGatewayBuilder msgGatewayBuilder;

   @Autowired
   public DbMsgTemplateApplication(MsgGatewayBuilder msgGatewayBuilder, MessageTemplateReadService messageTemplateReadService) {
      super(msgGatewayBuilder);
      this.msgGatewayBuilder = msgGatewayBuilder;
      this.messageTemplateReadService = messageTemplateReadService;
   }

   protected Template getTitleTemplate(String template, Integer channel, String deviceType) {
      MessageTemplate messageTemplate = this.getMessageTemplate(template, channel, deviceType);
      log.info("template={}", messageTemplate);
      if(messageTemplate == null) {
         return null;
      } else {
         try {
            return handlebars.compileInline(messageTemplate.getTitle());
         } catch (Exception var6) {
            log.warn("compile message title fail, template={}", messageTemplate);
            return null;
         }
      }
   }

   protected Template getContentTemplate(String template, Integer channel, String deviceType) {
      MessageTemplate messageTemplate = this.getMessageTemplate(template, channel, deviceType);
      log.info("template={}", messageTemplate);
      if(messageTemplate == null) {
         return null;
      } else {
         try {
            return handlebars.compileInline(messageTemplate.getContent());
         } catch (Exception var6) {
            log.warn("compile message content fail, template={}", messageTemplate);
            return null;
         }
      }
   }

   private MessageTemplate getMessageTemplate(String template, Integer channel, String deviceType) {
      MessageTemplate messageTemplate = null;
      String actualTemplate = super.getActualTemplateName(template, channel, deviceType);
      messageTemplate = (MessageTemplate)RespUtil.orJsonEx(this.messageTemplateReadService.findTemplateByName(actualTemplate), "findTemplateByName", new Object[]{actualTemplate});
      if(messageTemplate == null) {
         messageTemplate = (MessageTemplate)RespUtil.orJsonEx(this.messageTemplateReadService.findTemplateByName(template), "findTemplateByName", new Object[]{template});
      }

      return messageTemplate;
   }
}
