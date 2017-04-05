package io.terminus.parana.web.msg.impl.apppush.db;

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
   public AppPushWebService(@Qualifier("dbAppPushMsgReceiversTranslator") MsgReceiversTranslator msgReceiversTranslator, @Qualifier("dbAppPushMsgSendRequestHandler") MsgSendRequestHandler msgSendRequestHandler, @Qualifier("dbMsgSubscribeChecker") MsgSubscribeChecker msgSubscribeChecker, @Qualifier("dbMsgTemplateApplication") MsgTemplateApplication msgTemplateApplication, @Qualifier("appPushSender") MsgSender msgSender, @Qualifier("dbMsgSendResultHandler") MsgSendResultHandler msgSendResultHandler) {
      super(msgReceiversTranslator, msgSendRequestHandler, msgSubscribeChecker, msgTemplateApplication, msgSender, msgSendResultHandler);
   }
}
