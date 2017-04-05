package io.terminus.parana.web.msg.impl.apppush.simple;

import io.terminus.parana.web.msg.MsgReceiversTranslator;
import io.terminus.parana.web.msg.MsgSendRequestHandler;
import io.terminus.parana.web.msg.MsgSendResultHandler;
import io.terminus.parana.web.msg.MsgSender;
import io.terminus.parana.web.msg.MsgSubscribeChecker;
import io.terminus.parana.web.msg.MsgTemplateApplication;
import io.terminus.parana.web.msg.MsgWebService;
import io.terminus.parana.web.msg.impl.DefaultMsgWebServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class AppPushWebService extends DefaultMsgWebServiceImpl implements MsgWebService {
   @Autowired
   public AppPushWebService(@Qualifier("simpleMsgReceiversTranslator") MsgReceiversTranslator msgReceiversTranslator, @Qualifier("simpleAppPushMsgSendRequestHandler") MsgSendRequestHandler msgSendRequestHandler, @Qualifier("simpleMsgSubscribeChecker") MsgSubscribeChecker msgSubscribeChecker, @Qualifier("simpleMsgTemplateApplication") MsgTemplateApplication msgTemplateApplication, @Qualifier("appPushSender") MsgSender msgSender, @Qualifier("simpleMsgSendResultHandler") MsgSendResultHandler msgSendResultHandler) {
      super(msgReceiversTranslator, msgSendRequestHandler, msgSubscribeChecker, msgTemplateApplication, msgSender, msgSendResultHandler);
   }
}
