	package cn.blmdz.wolf.web.msg.admin.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.dto.MessageCriteria;
import cn.blmdz.wolf.msg.model.Message;
import cn.blmdz.wolf.msg.model.Message.Status;
import cn.blmdz.wolf.msg.service.MessageReadService;
import cn.blmdz.wolf.msg.service.MessageWriteService;

@Controller
@RequestMapping({"/api/msg/message"})
public class AdminMessages {
   private static final Logger log = LoggerFactory.getLogger(AdminMessages.class);
   private final MessageWriteService messageWriteService;
   private final MessageReadService messageReadService;

   @Autowired
   public AdminMessages(MessageReadService messageReadService, MessageWriteService messageWriteService) {
      this.messageReadService = messageReadService;
      this.messageWriteService = messageWriteService;
   }

   @RequestMapping(
      value = {""},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   @ResponseBody
   public Long createMessage(@RequestBody Message message) {
      Response<Long> response = this.messageWriteService.createMessage(message);
      if(response.isSuccess()) {
         return (Long)response.getResult();
      } else {
         log.error("createMessage failed, message={}, cause={}", message, response.getError());
         throw new JsonResponseException(response.getError());
      }
   }

   @RequestMapping(
      value = {"/close/{messageId}"},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   @ResponseBody
   public Boolean closeMessage(@PathVariable("messageId") Long messageId) {
      Message toUpdate = new Message();
      toUpdate.setId(messageId);
      toUpdate.setStatus(Integer.valueOf(Status.Closed.value()));
      Response<Boolean> response = this.messageWriteService.updateMessage(toUpdate);
      if(response.isSuccess()) {
         return (Boolean)response.getResult();
      } else {
         log.error("closeMessage failed, messageId={}, cause={}", messageId, response.getError());
         throw new JsonResponseException(response.getError());
      }
   }

   @RequestMapping(
      value = {"/paging"},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   @ResponseBody
   public Paging pagingMessages(MessageCriteria criteria) {
      Response<Paging<Message>> response = this.messageReadService.pagingMessages(criteria);
      if(response.isSuccess()) {
         return (Paging)response.getResult();
      } else {
         log.error("pagingMessages failed, criteria={}, cause={}", criteria, response.getError());
         throw new JsonResponseException(response.getError());
      }
   }

   @RequestMapping(
      value = {""},
      method = {RequestMethod.PUT},
      produces = {"application/json"}
   )
   @ResponseBody
   public Boolean updateMessage(@RequestBody Message message) {
      message.setStatus((Integer)null);
      Response<Boolean> response = this.messageWriteService.updateMessage(message);
      if(response.isSuccess()) {
         return (Boolean)response.getResult();
      } else {
         log.error("updateMessage failed, message={}, cause={}", message, response.getError());
         throw new JsonResponseException(response.getError());
      }
   }
}
