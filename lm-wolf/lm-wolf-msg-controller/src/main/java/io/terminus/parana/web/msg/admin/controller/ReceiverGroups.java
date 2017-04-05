package io.terminus.parana.web.msg.admin.controller;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.parana.msg.dto.ReceiverGroupCriteria;
import io.terminus.parana.msg.model.ReceiverGroup;
import io.terminus.parana.msg.service.ReceiverGroupReadService;
import io.terminus.parana.msg.service.ReceiverGroupWriteService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping({"/api/msg/group"})
public class ReceiverGroups {
   private static final Logger log = LoggerFactory.getLogger(ReceiverGroups.class);
   private final ReceiverGroupReadService receiverGroupReadService;
   private final ReceiverGroupWriteService receiverGroupWriteService;

   @Autowired
   public ReceiverGroups(ReceiverGroupReadService receiverGroupReadService, ReceiverGroupWriteService receiverGroupWriteService) {
      this.receiverGroupReadService = receiverGroupReadService;
      this.receiverGroupWriteService = receiverGroupWriteService;
   }

   @RequestMapping(
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   @ResponseBody
   public Long createReceiverGroup(@RequestBody ReceiverGroup receiverGroup) {
      Response<Long> response = this.receiverGroupWriteService.createReceiverGroup(receiverGroup);
      if(response.isSuccess()) {
         return (Long)response.getResult();
      } else {
         log.error("createReceiverGroup failed, group={}, cause={}", receiverGroup, response.getError());
         throw new JsonResponseException(response.getError());
      }
   }

   @RequestMapping(
      method = {RequestMethod.PUT},
      produces = {"application/json"}
   )
   @ResponseBody
   public Boolean updateReceiverGroup(@RequestBody ReceiverGroup receiverGroup) {
      Response<Boolean> response = this.receiverGroupWriteService.updateReceiverGroup(receiverGroup);
      if(response.isSuccess()) {
         return (Boolean)response.getResult();
      } else {
         log.error("updateReceiverGroup failed, group={}, cause={}", receiverGroup, response.getError());
         throw new JsonResponseException(response.getError());
      }
   }

   @RequestMapping(
      value = {"/{groupId}"},
      method = {RequestMethod.DELETE},
      produces = {"application/json"}
   )
   @ResponseBody
   public Boolean deleteReceiverGroup(@PathVariable("groupId") Long groupId) {
      Response<Boolean> response = this.receiverGroupWriteService.deleteReceiverGroup(groupId);
      if(response.isSuccess()) {
         return (Boolean)response.getResult();
      } else {
         log.error("deleteReceiverGroup failed, groupId={}, cause={}", groupId, response.getError());
         throw new JsonResponseException(response.getError());
      }
   }

   @RequestMapping(
      value = {"/paging"},
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   @ResponseBody
   public Paging pagingReceiverGroups(ReceiverGroupCriteria criteria) {
      Response<Paging<ReceiverGroup>> response = this.receiverGroupReadService.pagingReceiverGroups(criteria);
      if(response.isSuccess()) {
         return (Paging)response.getResult();
      } else {
         log.error("pagingReceiverGroup failed, criteria={}, cause={}", criteria, response.getError());
         throw new JsonResponseException(response.getError());
      }
   }
}
