package cn.blmdz.wolf.web.msg;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.wolf.msg.model.Message;

public interface MsgSendResultHandler {
   void handlerResult(Message var1) throws JsonResponseException;
}
