package io.terminus.parana.web.msg;

import io.terminus.common.model.Response;
import io.terminus.parana.msg.model.Message;

public interface MsgSender {
   Response send(Message var1);
}
