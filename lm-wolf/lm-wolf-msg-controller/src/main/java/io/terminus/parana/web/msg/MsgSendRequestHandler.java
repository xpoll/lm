package io.terminus.parana.web.msg;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.parana.msg.model.Message;
import java.util.Map;

public interface MsgSendRequestHandler {
   Message request(String var1, String var2, Map var3, String var4) throws JsonResponseException;

   Message request(String var1, String var2, String var3, String var4) throws JsonResponseException;

   Message request(Message var1) throws JsonResponseException;
}
