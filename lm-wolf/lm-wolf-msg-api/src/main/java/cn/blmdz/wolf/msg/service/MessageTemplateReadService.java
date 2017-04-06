package cn.blmdz.wolf.msg.service;

import java.util.Map;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.dto.MessageTemplateCriteria;

public interface MessageTemplateReadService {
   Response getMessageTitle(String var1, Map var2);

   Response getMessageContent(String var1, Map var2);

   Response getMessageInfo(String var1, Map var2);

   Response findTemplateById(Long var1);

   Response findTemplateByName(String var1);

   Response pagingTemplates(MessageTemplateCriteria var1);
}
