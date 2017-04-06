package cn.blmdz.wolf.web.msg;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.model.Message;

public interface MsgSender {
   Response send(Message var1);
}
