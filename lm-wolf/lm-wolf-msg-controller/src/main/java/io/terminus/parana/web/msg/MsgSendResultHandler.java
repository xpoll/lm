package io.terminus.parana.web.msg;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.parana.msg.model.Message;

public interface MsgSendResultHandler {
   void handlerResult(Message var1) throws JsonResponseException;
}
