package cn.blmdz.wolf.web.msg.impl.sms.db;

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
public class SmsWebService extends DefaultMsgWebServiceImpl implements MsgWebService {
   @Autowired
   public SmsWebService(@Qualifier("dbSmsMsgReceiversTranslator") MsgReceiversTranslator msgReceiversTranslator, @Qualifier("dbSmsMsgSendRequestHandler") MsgSendRequestHandler msgSendRequestHandler, @Qualifier("dbMsgSubscribeChecker") MsgSubscribeChecker msgSubscribeChecker, @Qualifier("dbMsgTemplateApplication") MsgTemplateApplication msgTemplateApplication, @Qualifier("smsSender") MsgSender msgSender, @Qualifier("dbMsgSendResultHandler") MsgSendResultHandler msgSendResultHandler) {
      super(msgReceiversTranslator, msgSendRequestHandler, msgSubscribeChecker, msgTemplateApplication, msgSender, msgSendResultHandler);
   }
}
