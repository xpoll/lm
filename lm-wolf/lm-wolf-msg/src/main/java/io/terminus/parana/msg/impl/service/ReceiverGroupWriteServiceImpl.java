package io.terminus.parana.msg.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.msg.impl.dao.mysql.ReceiverGroupDao;
import io.terminus.parana.msg.model.ReceiverGroup;
import io.terminus.parana.msg.service.ReceiverGroupWriteService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;

@Service
public class ReceiverGroupWriteServiceImpl implements ReceiverGroupWriteService {
   private static final Logger log = LoggerFactory.getLogger(ReceiverGroupWriteServiceImpl.class);
   private final ReceiverGroupDao receiverGroupDao;

   @Autowired
   public ReceiverGroupWriteServiceImpl(ReceiverGroupDao receiverGroupDao) {
      this.receiverGroupDao = receiverGroupDao;
   }

   public Response createReceiverGroup(ReceiverGroup receiverGroup) {
      Response<Long> response = new Response();

      try {
         this.receiverGroupDao.create(receiverGroup);
         response.setResult(receiverGroup.getId());
      } catch (DuplicateKeyException var4) {
         log.error("createReceiverGroup failed, params={}, cause={}", receiverGroup, Throwables.getStackTraceAsString(var4));
         response.setError("create.receiver.group.failed.by.duplicate.key");
      } catch (Exception var5) {
         log.error("createReceiverGroup failed, params={}, cause={}", receiverGroup, Throwables.getStackTraceAsString(var5));
         response.setError("create.receiver.group.failed");
      }

      return response;
   }

   public Response updateReceiverGroup(ReceiverGroup receiverGroup) {
      Response<Boolean> response = new Response();

      try {
         Boolean result = this.receiverGroupDao.update(receiverGroup);
         response.setResult(result);
      } catch (Exception var4) {
         log.error("updateReceiverGroup failed, params={}, cause={}", receiverGroup, Throwables.getStackTraceAsString(var4));
         response.setError("update.receiver.group.failed");
      }

      return response;
   }

   public Response deleteReceiverGroup(Long receiverGroupId) {
      Response<Boolean> response = new Response();

      try {
         Boolean result = this.receiverGroupDao.delete(receiverGroupId);
         response.setResult(result);
      } catch (Exception var4) {
         log.error("deleteReceiverGroup failed, id={}, cause={}", receiverGroupId, Throwables.getStackTraceAsString(var4));
         response.setError("delete.receiver.group.failed");
      }

      return response;
   }
}
