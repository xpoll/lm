package io.terminus.parana.pay.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.pay.model.AlipayTrans;
import java.util.Date;
import java.util.List;
import org.springframework.stereotype.Repository;

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
