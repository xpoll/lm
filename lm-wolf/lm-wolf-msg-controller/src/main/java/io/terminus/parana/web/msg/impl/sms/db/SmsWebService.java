package io.terminus.parana.web.msg.impl.sms.db;

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
public class SmsWebService extends DefaultMsgWebServiceImpl implements MsgWebService {
   @Autowired
   public SmsWebService(@Qualifier("dbSmsMsgReceiversTranslator") MsgReceiversTranslator msgReceiversTranslator, @Qualifier("dbSmsMsgSendRequestHandler") MsgSendRequestHandler msgSendRequestHandler, @Qualifier("dbMsgSubscribeChecker") MsgSubscribeChecker msgSubscribeChecker, @Qualifier("dbMsgTemplateApplication") MsgTemplateApplication msgTemplateApplication, @Qualifier("smsSender") MsgSender msgSender, @Qualifier("dbMsgSendResultHandler") MsgSendResultHandler msgSendResultHandler) {
      super(msgReceiversTranslator, msgSendRequestHandler, msgSubscribeChecker, msgTemplateApplication, msgSender, msgSendResultHandler);
   }
}
