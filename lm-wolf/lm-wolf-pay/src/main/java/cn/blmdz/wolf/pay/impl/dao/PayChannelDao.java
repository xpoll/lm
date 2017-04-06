package cn.blmdz.wolf.pay.impl.dao;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.pay.enums.PayChannelStatus;
import cn.blmdz.wolf.pay.enums.PayChannelType;
import cn.blmdz.wolf.pay.model.PayChannel;

@Repository
public class PayChannelDao extends MyBatisDao<PayChannel> {
   public List findByStageIdAndTypeAndStatus(Long stageId, PayChannelType type, Integer status) {
      return this.getSqlSession().selectList(this.sqlId("findByStageIdAndTypeAndStatus"), ImmutableMap.of("stageId", stageId, "status", status, "type", Integer.valueOf(type.value())));
   }

   public PayChannel findPaidPayChannelByStageId(Long stageId) {
      return (PayChannel)this.getSqlSession().selectOne(this.sqlId("findPaidPayChannelByStageId"), ImmutableMap.of("stageId", stageId, "status", Integer.valueOf(1), "type", Integer.valueOf(PayChannelType.PAID.value())));
   }

   public PayChannel findByTradeNo(String tradeNo, PayChannelType type) {
      return (PayChannel)this.getSqlSession().selectOne(this.sqlId("findByTradeNo"), ImmutableMap.of("tradeNo", tradeNo, "type", Integer.valueOf(type.value())));
   }

   public PayChannel findByPaymentCode(String paymentCode, PayChannelType type) {
      return (PayChannel)this.getSqlSession().selectOne(this.sqlId("findByPaymentCode"), ImmutableMap.of("paymentCode", paymentCode, "type", Integer.valueOf(type.value())));
   }

   public PayChannel findByBatchNo(String batchNo) {
      return (PayChannel)this.getSqlSession().selectOne(this.sqlId("findByBatchNo"), batchNo);
   }

   public PayChannel findByStageIdAndChannel(Long stageId, String channel, PayChannelType type) {
      return (PayChannel)this.getSqlSession().selectOne(this.sqlId("findByStageIdAndChannel"), ImmutableMap.of("stageId", stageId, "channel", channel, "type", Integer.valueOf(type.value())));
   }

   public List findByStageIdAndPaid(List payStageIds) {
      return this.getSqlSession().selectList(this.sqlId("findByStageIdAndPaid"), payStageIds);
   }

   public Paging pagingNeedGenerateChannelDetails(Integer offset, Integer limit) {
      Map<String, Object> map = Maps.newHashMap();
      map.put("status", Integer.valueOf(PayChannelStatus.FINISH.value()));
      map.put("isCreatedDetail", Integer.valueOf(0));
      Long count = (Long)this.getSqlSession().selectOne(this.sqlId("countNeedNeedSumChannelDetail"), map);
      if(count.longValue() == 0L) {
         return new Paging(Long.valueOf(0L), Collections.emptyList());
      } else {
         map.put("offset", offset);
         map.put("limit", limit);
         List<PayChannel> channels = this.getSqlSession().selectList(this.sqlId("pagingNeedNeedSumChannelDetail"), map);
         return new Paging(count, channels);
      }
   }

   public PayChannel findRefundByBatchNoAndStatus(String batchNo, PayChannelStatus status) {
      return (PayChannel)this.getSqlSession().selectOne(this.sqlId("findRefundByBatchNoAndStatus"), ImmutableMap.of("batchNo", batchNo, "status", Integer.valueOf(status.value())));
   }

   public Boolean setRefundSuccess(String batchNo, PayChannelStatus from, PayChannelStatus to, Date date) {
      return Boolean.valueOf(this.getSqlSession().update(this.sqlId("setRefundSuccess"), ImmutableMap.of("batchNo", batchNo, "from", Integer.valueOf(from.value()), "to", Integer.valueOf(to.value()), "refundAt", date)) > 0);
   }

   public PayChannel findRefundByRefundIdAndChannel(Long refundOrderId, String channel) {
      return (PayChannel)this.getSqlSession().selectOne(this.sqlId("findRefundByRefundIdAndChannel"), ImmutableMap.of("refundOrderId", refundOrderId, "channel", channel));
   }
}
