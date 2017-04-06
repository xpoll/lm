package cn.blmdz.wolf.msg.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.model.MessageTemplate;

public interface MessageTemplateWriteService {
   Response createTemplate(MessageTemplate var1);

   Response updateTemplate(MessageTemplate var1);

   Response deleteTemplateById(Long var1);
}
