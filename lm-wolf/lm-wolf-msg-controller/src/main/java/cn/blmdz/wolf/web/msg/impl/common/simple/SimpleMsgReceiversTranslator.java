package cn.blmdz.wolf.web.msg.impl.common.simple;

import org.springframework.stereotype.Component;

import cn.blmdz.wolf.web.msg.MsgReceiversTranslator;

@Component
public class SimpleMsgReceiversTranslator implements MsgReceiversTranslator {
   public String translateReceivers(String receivers) {
      return receivers;
   }
}
