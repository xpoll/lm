package io.terminus.parana.msg.component;

import io.terminus.common.exception.ServiceException;
import io.terminus.parana.msg.model.Message;

public interface MessageValidatorChain {
   void check(Message var1) throws ServiceException;
}
