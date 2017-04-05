package io.terminus.parana.pay.mock.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.pay.mock.model.MockWechatpayTrans;
import java.util.Date;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class MockWechatpayTransDao extends MyBatisDao {
   public MockWechatpayTrans findByOutTradeNoAndTradeStatus(String outTradeNo, String status) {
      return (MockWechatpayTrans)this.getSqlSession().selectOne(this.sqlId("findByOutTradeNoAndTradeStatus"), ImmutableMap.of("outTradeNo", outTradeNo, "status", status));
   }

   public MockWechatpayTrans findByOutRefundNoAndRefundStatus(String outRefundNo, String status) {
      return (MockWechatpayTrans)this.getSqlSession().selectOne(this.sqlId("findByOutRefundNoAndRefundStatus"), ImmutableMap.of("outRefundNo", outRefundNo, "status", status));
   }

   public Long createNotExists(MockWechatpayTrans wechatpayTrans) {
      int count = this.countByOutTradeNo(wechatpayTrans.getOutTradeNo(), wechatpayTrans.getTradeTime());
      if(count > 0) {
         return Long.valueOf(0L);
      } else {
         this.create(wechatpayTrans);
         return wechatpayTrans.getId();
      }
   }

   public int countByOutTradeNo(String outTradeNo, String tradeTime) {
      return ((Integer)this.getSqlSession().selectOne("countByOutTradeNo", ImmutableMap.of("outTradeNo", outTradeNo, "tradeTime", tradeTime))).intValue();
   }

   public List findByOutTradeNo(String outTradeNo) {
      return this.getSqlSession().selectList(this.sqlId("findByOutTradeNo"), outTradeNo);
   }

   public List findByTransactionId(String transactionId) {
      return this.getSqlSession().selectList(this.sqlId("findByTransactionId"), transactionId);
   }

   public List findIncomeFee(Date startAt, Date endAt) {
      return this.getSqlSession().selectList(this.sqlId("findIncomeFee"), ImmutableMap.of("startAt", startAt, "endAt", endAt));
   }
}
