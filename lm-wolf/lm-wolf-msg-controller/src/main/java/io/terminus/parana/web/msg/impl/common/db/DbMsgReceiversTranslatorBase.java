package io.terminus.parana.web.msg.impl.common.db;

import io.terminus.parana.web.msg.MsgReceiversTranslator;

public abstract class DbMsgReceiversTranslatorBase implements MsgReceiversTranslator {
   public String translateReceivers(String receivers) {
      return receivers;
   }
}
