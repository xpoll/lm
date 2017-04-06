package cn.blmdz.wolf.settlement.impl.dao;

import java.util.Collections;
import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.base.Function;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.settlement.enums.CheckStatus;
import cn.blmdz.wolf.settlement.enums.PayChannelTransType;
import cn.blmdz.wolf.settlement.model.PayChannelDetail;

@Repository
public class PayChannelDetailDao extends MyBatisDao {
   public PayChannelDetail findBySystemNo(String systemNo) {
      return (PayChannelDetail)this.getSqlSession().selectOne(this.sqlId("loadBySystemNo"), systemNo);
   }

   public PayChannelDetail findByRefundOrderId(Long refundOrderId) {
      return (PayChannelDetail)this.getSqlSession().selectOne(this.sqlId("findByRefundOrderId"), refundOrderId);
   }

   public PayChannelDetail findPayChannelDetailByChannelAndPaymentCode(String channel, String paymentCode) {
      return (PayChannelDetail)this.getSqlSession().selectOne(this.sqlId("findPayChannelSumDetailByChannelAndPaymentCodeAndType"), ImmutableMap.of("channel", channel, "paymentCode", paymentCode, "type", Integer.valueOf(PayChannelTransType.PAID.value())));
   }

   public PayChannelDetail findPayChannelDetailByChannelAndBatchNo(String channel, String batchNo) {
      return (PayChannelDetail)this.getSqlSession().selectOne(this.sqlId("findPayChannelSumDetailByChannelAndBatchNo"), ImmutableMap.of("channel", channel, "batchNo", batchNo));
   }

   public Paging pagingNeedCheck(Integer offset, Integer limit) {
      Long count = (Long)this.getSqlSession().selectOne(this.sqlId("countNeedCheck"), ImmutableMap.of("status", Integer.valueOf(CheckStatus.CHECK_SUCCESS.value())));
      if(count.longValue() == 0L) {
         return new Paging(Long.valueOf(0L), Collections.emptyList());
      } else {
         List<PayChannelDetail> orderTracks = this.getSqlSession().selectList(this.sqlId("pagingNeedCheck"), ImmutableMap.of("status", Integer.valueOf(CheckStatus.CHECK_SUCCESS.value()), "offset", offset, "limit", limit));
         return new Paging(count, orderTracks);
      }
   }

   public Boolean updateStatus(Long id, CheckStatus status) {
      return Boolean.valueOf(this.getSqlSession().update(this.sqlId("updateStatus"), ImmutableMap.of("id", id, "status", Integer.valueOf(status.value()))) > 0);
   }

   public List getPcsdBySystemNoAndStatusAndType(String systemNo, List status, PayChannelTransType type) {
      List<Integer> sts = Lists.transform(status, new Function<CheckStatus, Integer>() {
         public Integer apply(CheckStatus input) {
            return Integer.valueOf(input.value());
         }
      });
      return this.getSqlSession().selectList(this.sqlId("getPcsdBySystemNoAndStatusAndType"), ImmutableMap.of("systemNo", systemNo, "status", sts, "type", Integer.valueOf(type.value())));
   }
}
