package cn.blmdz.wolf.web.msg.impl.common.simple;

import org.springframework.stereotype.Component;

import cn.blmdz.wolf.msg.model.Message;
import cn.blmdz.wolf.web.msg.MsgSubscribeChecker;

@Component
public class SimpleMsgSubscribeChecker implements MsgSubscribeChecker {
   public void checkSubscribe(Message message) {
   }
}
