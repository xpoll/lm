package io.terminus.parana.msg.service;

import io.terminus.common.model.Response;
import io.terminus.parana.msg.model.MessageTemplate;

public interface MessageTemplateWriteService {
   Response createTemplate(MessageTemplate var1);

   Response updateTemplate(MessageTemplate var1);

   Response deleteTemplateById(Long var1);
}
