package cn.blmdz.wolf.msg.impl.service;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.wolf.msg.dto.ReceiverGroupCriteria;
import cn.blmdz.wolf.msg.impl.dao.mysql.ReceiverGroupDao;
import cn.blmdz.wolf.msg.model.ReceiverGroup;
import cn.blmdz.wolf.msg.service.ReceiverGroupReadService;

@Service
public class ReceiverGroupReadServiceImpl implements ReceiverGroupReadService {
   private static final Logger log = LoggerFactory.getLogger(ReceiverGroupReadServiceImpl.class);
   private final ReceiverGroupDao receiverGroupDao;

   @Autowired
   public ReceiverGroupReadServiceImpl(ReceiverGroupDao receiverGroupDao) {
      this.receiverGroupDao = receiverGroupDao;
   }

   public Response findReceiverGroupById(Long receiverGroupId) {
      Response<ReceiverGroup> response = new Response();

      try {
         ReceiverGroup group = (ReceiverGroup)this.receiverGroupDao.findById(receiverGroupId);
         response.setResult(group);
      } catch (Exception var4) {
         log.error("findReceiverGroupById failed, id={}, cause={}", receiverGroupId, Throwables.getStackTraceAsString(var4));
         response.setError("find.receiver.group.by.id.failed");
      }

      return response;
   }

   public Response findReceiverGroupByUserId(Long userId) {
      Response<ReceiverGroup> response = new Response();

      try {
         ReceiverGroup group = this.receiverGroupDao.findByUserId(userId);
         response.setResult(group);
      } catch (Exception var4) {
         log.error("findReceiverGroupByUserId failed, id={}, cause={}", userId, Throwables.getStackTraceAsString(var4));
         response.setError("find.receiver.group.by.user.id.failed");
      }

      return response;
   }

   public Response pagingReceiverGroups(ReceiverGroupCriteria criteria) {
      Response<Paging<ReceiverGroup>> response = new Response();

      try {
         Paging<ReceiverGroup> page = this.receiverGroupDao.paging((Map)BeanMapper.map(criteria, Map.class));
         response.setResult(page);
      } catch (Exception var4) {
         log.error("pagingReceiverGroups failed, criteria={}, cause={}", criteria, Throwables.getStackTraceAsString(var4));
         response.setError("paging.receiver.groups.failed");
      }

      return response;
   }
}
