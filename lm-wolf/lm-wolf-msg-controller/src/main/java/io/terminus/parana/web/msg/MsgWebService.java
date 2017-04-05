package io.terminus.parana.web.msg;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.parana.msg.model.Message;
import java.util.Map;

public interface MsgWebService {
   String send(String var1, String var2, Map var3, String var4) throws JsonResponseException;

   String send(String var1, String var2, String var3, String var4) throws JsonResponseException;

   String send(Message var1) throws JsonResponseException;
}
