package io.terminus.parana.pay.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.pay.model.PayStage;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class PayStageDao extends MyBatisDao {
   public List findByPayId(Long payId) {
      return this.getSqlSession().selectList(this.sqlId("findByPayId"), payId);
   }

   public PayStage findByPayIdAndStage(Long payId, Integer currentStage) {
      return (PayStage)this.getSqlSession().selectOne("findByPayIdAndStage", ImmutableMap.of("payId", payId, "currentStage", currentStage));
   }

   public PayStage checkIsPaid(Long payId, List stages) {
      return (PayStage)this.getSqlSession().selectOne("checkIsPaid", ImmutableMap.of("payId", payId, "stages", stages));
   }

   public Integer sumFeeByPayId(Long payId) {
      return (Integer)this.getSqlSession().selectOne("sumFeeByPayId", payId);
   }

   public Integer countNoPay(Long payId, Integer paidStatus) {
      return (Integer)this.getSqlSession().selectOne(this.sqlId("countNoPay"), ImmutableMap.of("payId", payId, "paidStatus", paidStatus));
   }

   public PayStage findRecentStageByPaySystemNo(String systemNo) {
      return (PayStage)this.getSqlSession().selectOne(this.sqlId("findRecentStageByPaySystemNo"), systemNo);
   }

   public PayStage findByPayPaymentCode(String paymentCode) {
      return (PayStage)this.getSqlSession().selectOne(this.sqlId("findByPayPaymentCode"), paymentCode);
   }

   public List loadBySystemNo(String systemNo) {
      return this.getSqlSession().selectList(this.sqlId("loadBySystemNo"), systemNo);
   }
}
