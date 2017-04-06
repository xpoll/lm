package cn.blmdz.wolf.web.msg;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.wolf.msg.model.Message;

public interface MsgSubscribeChecker {
   void checkSubscribe(Message var1) throws JsonResponseException;
}
