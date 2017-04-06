package cn.blmdz.wolf.web.msg.impl.common.db;

import cn.blmdz.wolf.web.msg.MsgReceiversTranslator;

public abstract class DbMsgReceiversTranslatorBase implements MsgReceiversTranslator {
   public String translateReceivers(String receivers) {
      return receivers;
   }
}
