package cn.blmdz.wolf.web.msg.impl.notify.db;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
public class NotifyWebService extends DefaultMsgWebServiceImpl implements MsgWebService {
   private static final Logger log = LoggerFactory.getLogger(NotifyWebService.class);

   @Autowired
   public NotifyWebService(@Qualifier("dbNotifyMsgReceiversTranslator") MsgReceiversTranslator msgReceiversTranslator, @Qualifier("dbNotifyMsgSendRequestHandler") MsgSendRequestHandler msgSendRequestHandler, @Qualifier("dbMsgSubscribeChecker") MsgSubscribeChecker msgSubscribeChecker, @Qualifier("dbMsgTemplateApplication") MsgTemplateApplication msgTemplateApplication, @Qualifier("notifySender") MsgSender msgSender, @Qualifier("dbMsgSendResultHandler") MsgSendResultHandler msgSendResultHandler) {
      super(msgReceiversTranslator, msgSendRequestHandler, msgSubscribeChecker, msgTemplateApplication, msgSender, msgSendResultHandler);
   }
}
