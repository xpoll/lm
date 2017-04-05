package io.terminus.parana.web.msg.impl;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.parana.msg.model.Message;
import io.terminus.parana.msg.model.Message.Status;
import io.terminus.parana.web.msg.MsgReceiversTranslator;
import io.terminus.parana.web.msg.MsgSendRequestHandler;
import io.terminus.parana.web.msg.MsgSendResultHandler;
import io.terminus.parana.web.msg.MsgSender;
import io.terminus.parana.web.msg.MsgSubscribeChecker;
import io.terminus.parana.web.msg.MsgTemplateApplication;
import io.terminus.parana.web.msg.MsgWebService;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class DefaultMsgWebServiceImpl implements MsgWebService {
   private static final Logger log = LoggerFactory.getLogger(DefaultMsgWebServiceImpl.class);
   protected MsgReceiversTranslator msgReceiversTranslator;
   protected MsgSendRequestHandler msgSendRequestHandler;
   protected MsgSubscribeChecker msgSubscribeChecker;
   protected MsgTemplateApplication msgTemplateApplication;
   protected MsgSender msgSender;
   protected MsgSendResultHandler msgSendResultHandler;

   public DefaultMsgWebServiceImpl(MsgReceiversTranslator msgReceiversTranslator, MsgSendRequestHandler msgSendRequestHandler, MsgSubscribeChecker msgSubscribeChecker, MsgTemplateApplication msgTemplateApplication, MsgSender msgSender, MsgSendResultHandler msgSendResultHandler) {
      this.msgReceiversTranslator = msgReceiversTranslator;
      this.msgSendRequestHandler = msgSendRequestHandler;
      this.msgSubscribeChecker = msgSubscribeChecker;
      this.msgTemplateApplication = msgTemplateApplication;
      this.msgSender = msgSender;
      this.msgSendResultHandler = msgSendResultHandler;
   }

   public String send(String toes, String templateName, Map context, String extra) {
      toes = this.msgReceiversTranslator.translateReceivers(toes);
      Message message = this.msgSendRequestHandler.request(toes, templateName, context, extra);
      return this.doSendMessage(message);
   }

   public String send(String toes, String title, String content, String extra) {
      toes = this.msgReceiversTranslator.translateReceivers(toes);
      Message message = this.msgSendRequestHandler.request(toes, title, content, extra);
      return this.doSendMessage(message);
   }

   public String send(Message message) {
      String toes = this.msgReceiversTranslator.translateReceivers(message.getReceivers());
      message.setReceivers(toes);
      return this.doSendMessage(message);
   }

   protected String doSendMessage(Message message) {
      List<Message> messageList;
      try {
         this.msgSubscribeChecker.checkSubscribe(message);
         messageList = this.msgTemplateApplication.applyTemplate(message);
      } catch (JsonResponseException var9) {
         message.setStatus(Integer.valueOf(Status.SendFailed.value()));
         message.setFailReason(var9.getMessage());
         this.msgSendResultHandler.handlerResult(message);
         throw var9;
      }

      StringBuilder sbSuccess = new StringBuilder();
      StringBuilder sbFail = new StringBuilder();
      Boolean flag = Boolean.valueOf(true);

      for(Message m : messageList) {
         Response<String> result = this.msgSender.send(m);
         if(result.isSuccess()) {
            sbSuccess.append((String)result.getResult()).append("\n");
         } else {
            sbFail.append(result.getError()).append("\n");
            flag = Boolean.valueOf(false);
         }
      }

      if(flag.booleanValue()) {
         message.setChannelOutput(sbSuccess.toString());
         message.setStatus(Integer.valueOf(Status.SendSuccess.value()));
      } else {
         message.setStatus(Integer.valueOf(Status.SendFailed.value()));
         message.setChannelOutput(sbSuccess.toString());
         message.setFailReason(sbFail.toString());
      }

      this.msgSendResultHandler.handlerResult(message);
      if(flag.booleanValue()) {
         return sbSuccess.toString();
      } else {
         throw new JsonResponseException(sbFail.toString());
      }
   }
}
