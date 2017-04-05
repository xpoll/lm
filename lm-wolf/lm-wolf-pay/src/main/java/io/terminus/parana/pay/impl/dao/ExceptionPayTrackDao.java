package io.terminus.parana.pay.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.pay.model.ExceptionPayTrack;
import org.springframework.stereotype.Repository;

@Repository
public class ExceptionPayTrackDao extends MyBatisDao {
   public ExceptionPayTrack findByTradeNo(String tradeNo) {
      return (ExceptionPayTrack)this.getSqlSession().selectOne(this.sqlId("findByTradeNo"), ImmutableMap.of("tradeNo", tradeNo));
   }

   public ExceptionPayTrack findByPaymentCode(String paymentCode) {
      return (ExceptionPayTrack)this.getSqlSession().selectOne(this.sqlId("findByPaymentCode"), ImmutableMap.of("paymentCode", paymentCode));
   }
}
