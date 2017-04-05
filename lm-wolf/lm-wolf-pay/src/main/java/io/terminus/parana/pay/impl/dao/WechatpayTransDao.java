package io.terminus.parana.pay.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.pay.model.WechatpayTrans;
import java.util.Date;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class WechatpayTransDao extends MyBatisDao {
   public Long createNotExists(WechatpayTrans wechatpayTrans) {
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
