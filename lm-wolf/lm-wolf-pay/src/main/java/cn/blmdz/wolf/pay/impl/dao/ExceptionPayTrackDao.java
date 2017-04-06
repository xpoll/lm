package cn.blmdz.wolf.pay.impl.dao;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.pay.model.ExceptionPayTrack;

@Repository
public class ExceptionPayTrackDao extends MyBatisDao<ExceptionPayTrack> {
   public ExceptionPayTrack findByTradeNo(String tradeNo) {
      return (ExceptionPayTrack)this.getSqlSession().selectOne(this.sqlId("findByTradeNo"), ImmutableMap.of("tradeNo", tradeNo));
   }

   public ExceptionPayTrack findByPaymentCode(String paymentCode) {
      return (ExceptionPayTrack)this.getSqlSession().selectOne(this.sqlId("findByPaymentCode"), ImmutableMap.of("paymentCode", paymentCode));
   }
}
