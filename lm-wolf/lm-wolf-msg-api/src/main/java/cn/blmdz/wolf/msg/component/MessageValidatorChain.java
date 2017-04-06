package cn.blmdz.wolf.msg.component;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.wolf.msg.model.Message;

public interface MessageValidatorChain {
   void check(Message var1) throws ServiceException;
}
