package io.terminus.parana.msg.service;

import io.terminus.common.model.Response;
import io.terminus.parana.msg.dto.MessageTemplateCriteria;
import java.util.Map;

public interface MessageTemplateReadService {
   Response getMessageTitle(String var1, Map var2);

   Response getMessageContent(String var1, Map var2);

   Response getMessageInfo(String var1, Map var2);

   Response findTemplateById(Long var1);

   Response findTemplateByName(String var1);

   Response pagingTemplates(MessageTemplateCriteria var1);
}
