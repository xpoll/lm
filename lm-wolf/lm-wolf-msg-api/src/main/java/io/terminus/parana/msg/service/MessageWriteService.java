package io.terminus.parana.msg.service;

import io.terminus.common.model.Response;
import io.terminus.parana.msg.model.Message;
import java.util.Map;

public interface MessageWriteService {
   Response sendMessage(Integer var1, String var2, String var3, String var4, String var5);

   Response sendMessageViaTemplate(Integer var1, String var2, Map var3, String var4, String var5);

   Response createMessage(Message var1);

   Response updateMessage(Message var1);
}
