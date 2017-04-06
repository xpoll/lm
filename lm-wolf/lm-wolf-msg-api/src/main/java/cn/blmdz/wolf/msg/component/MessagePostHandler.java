package cn.blmdz.wolf.msg.component;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.wolf.msg.model.Message;

public interface MessagePostHandler {
   void postHandle(Message var1) throws ServiceException;
}
