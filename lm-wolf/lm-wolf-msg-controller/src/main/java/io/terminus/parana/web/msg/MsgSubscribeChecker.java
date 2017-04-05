package io.terminus.parana.web.msg;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.parana.msg.model.Message;

public interface MsgSubscribeChecker {
   void checkSubscribe(Message var1) throws JsonResponseException;
}
