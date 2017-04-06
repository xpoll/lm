package cn.blmdz.wolf.web.msg.impl.apppush.simple;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import cn.blmdz.wolf.web.msg.MsgReceiversTranslator;
import cn.blmdz.wolf.web.msg.MsgSendRequestHandler;
import cn.blmdz.wolf.web.msg.MsgSendResultHandler;
import cn.blmdz.wolf.web.msg.MsgSender;
import cn.blmdz.wolf.web.msg.MsgSubscribeChecker;
import cn.blmdz.wolf.web.msg.MsgTemplateApplication;
import cn.blmdz.wolf.web.msg.MsgWebService;
import cn.blmdz.wolf.web.msg.impl.DefaultMsgWebServiceImpl;

@Service
public class AppPushWebService extends DefaultMsgWebServiceImpl implements MsgWebService {
   @Autowired
   public AppPushWebService(@Qualifier("simpleMsgReceiversTranslator") MsgReceiversTranslator msgReceiversTranslator, @Qualifier("simpleAppPushMsgSendRequestHandler") MsgSendRequestHandler msgSendRequestHandler, @Qualifier("simpleMsgSubscribeChecker") MsgSubscribeChecker msgSubscribeChecker, @Qualifier("simpleMsgTemplateApplication") MsgTemplateApplication msgTemplateApplication, @Qualifier("appPushSender") MsgSender msgSender, @Qualifier("simpleMsgSendResultHandler") MsgSendResultHandler msgSendResultHandler) {
      super(msgReceiversTranslator, msgSendRequestHandler, msgSubscribeChecker, msgTemplateApplication, msgSender, msgSendResultHandler);
   }
}
