package cn.blmdz.wolf.web.msg.impl.common.simple;

import com.github.jknack.handlebars.Template;

import cn.blmdz.wolf.web.msg.MsgGatewayBuilder;
import cn.blmdz.wolf.web.msg.MsgTemplateApplication;
import cn.blmdz.wolf.web.msg.impl.common.MsgTemplateApplicationBase;

import java.io.IOException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SimpleMsgTemplateApplication extends MsgTemplateApplicationBase implements MsgTemplateApplication {
   private static final Logger log = LoggerFactory.getLogger(SimpleMsgTemplateApplication.class);

   @Autowired
   public SimpleMsgTemplateApplication(MsgGatewayBuilder msgGatewayBuilder) {
      super(msgGatewayBuilder);
   }

   protected Template getTitleTemplate(String template, Integer channel, String deviceType) {
      Template handlebarTemplate = null;
      String actualTemplateName = "template/title/" + this.getActualTemplateName(template, channel, deviceType);

      try {
         handlebarTemplate = handlebars.compile(actualTemplateName);
      } catch (IOException var8) {
         log.info("compile title template fail, template={}", actualTemplateName);
      }

      if(handlebarTemplate == null) {
         actualTemplateName = "template/title/" + template;

         try {
            handlebarTemplate = handlebars.compile("template/title/" + actualTemplateName);
         } catch (IOException var7) {
            log.info("compile title template fail, template={}", actualTemplateName);
         }
      }

      return handlebarTemplate;
   }

   protected Template getContentTemplate(String template, Integer channel, String deviceType) {
      Template handlebarTemplate = null;
      String actualTemplateName = "template/content/" + this.getActualTemplateName(template, channel, deviceType);

      try {
         handlebarTemplate = handlebars.compile(actualTemplateName);
      } catch (IOException var8) {
         log.info("compile content template fail, template={}", actualTemplateName);
      }

      if(handlebarTemplate == null) {
         actualTemplateName = "template/content/" + template;

         try {
            handlebarTemplate = handlebars.compile("template/content/" + actualTemplateName);
         } catch (IOException var7) {
            log.info("compile content template fail, template={}", actualTemplateName);
         }
      }

      return handlebarTemplate;
   }
}
