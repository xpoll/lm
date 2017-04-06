package cn.blmdz.wolf.pay.impl.dao;

import java.util.Date;
import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.pay.model.AlipayTrans;

@Repository
public class AlipayTransDao extends MyBatisDao {
   public Boolean createNonexists(AlipayTrans alipayTrans) {
      int countAccountLogId = ((Integer)this.getSqlSession().selectOne("countAccountLogId", alipayTrans.getIwAccountLogId())).intValue();
      return countAccountLogId > 0?Boolean.TRUE:this.create(alipayTrans);
   }

   public List findByTradeNo(String paymentCode) {
      return this.getSqlSession().selectList(this.sqlId("findByTradeNo"), paymentCode);
   }

   public List findByMerchantNo(String merchantOuterTradeNo) {
      return this.getSqlSession().selectList(this.sqlId("findByMerchantNo"), merchantOuterTradeNo);
   }

   public List findIncomeFee(Date startAt, Date endAt) {
      return this.getSqlSession().selectList(this.sqlId("findIncomeFee"), ImmutableMap.of("startAt", startAt, "endAt", endAt));
   }
}
