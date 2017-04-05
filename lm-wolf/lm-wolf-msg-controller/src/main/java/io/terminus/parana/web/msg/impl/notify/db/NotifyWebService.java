package io.terminus.parana.web.msg.impl.notify.db;

import io.terminus.parana.web.msg.MsgReceiversTranslator;
import io.terminus.parana.web.msg.MsgSendRequestHandler;
import io.terminus.parana.web.msg.MsgSendResultHandler;
import io.terminus.parana.web.msg.MsgSender;
import io.terminus.parana.web.msg.MsgSubscribeChecker;
import io.terminus.parana.web.msg.MsgTemplateApplication;
import io.terminus.parana.web.msg.MsgWebService;
import io.terminus.parana.web.msg.impl.DefaultMsgWebServiceImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class NotifyWebService extends DefaultMsgWebServiceImpl implements MsgWebService {
   private static final Logger log = LoggerFactory.getLogger(NotifyWebService.class);

   @Autowired
   public NotifyWebService(@Qualifier("dbNotifyMsgReceiversTranslator") MsgReceiversTranslator msgReceiversTranslator, @Qualifier("dbNotifyMsgSendRequestHandler") MsgSendRequestHandler msgSendRequestHandler, @Qualifier("dbMsgSubscribeChecker") MsgSubscribeChecker msgSubscribeChecker, @Qualifier("dbMsgTemplateApplication") MsgTemplateApplication msgTemplateApplication, @Qualifier("notifySender") MsgSender msgSender, @Qualifier("dbMsgSendResultHandler") MsgSendResultHandler msgSendResultHandler) {
      super(msgReceiversTranslator, msgSendRequestHandler, msgSubscribeChecker, msgTemplateApplication, msgSender, msgSendResultHandler);
   }
}
