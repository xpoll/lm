package io.terminus.parana.pay.impl.manager;

import com.google.common.base.Preconditions;
import io.terminus.parana.pay.impl.dao.PayChannelDao;
import io.terminus.parana.pay.impl.dao.PayStageDao;
import io.terminus.parana.pay.impl.dao.TradePayDao;
import io.terminus.parana.pay.model.PayChannel;
import io.terminus.parana.pay.model.PayStage;
import io.terminus.parana.pay.model.TradePay;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
public class PayManager {
   private static final Logger log = LoggerFactory.getLogger(PayManager.class);
   @Autowired
   private PayStageDao payStageDao;
   @Autowired
   private TradePayDao tradePayDao;
   @Autowired
   private PayChannelDao payChannelDao;

   @Transactional
   public void updateStageFee(TradePay tradePay, List payStageList) {
      for(PayStage payStage : payStageList) {
         this.payStageDao.update(payStage);
      }

      Integer oldFee = tradePay.getAlreadyFee();
      Integer newFee = this.payStageDao.sumFeeByPayId(tradePay.getId());
      Preconditions.checkState(oldFee.intValue() < newFee.intValue(), "update.stage.fee.invalid");
      TradePay tp = new TradePay();
      tp.setId(tradePay.getId());
      tp.setShouldFee(newFee);
      this.tradePayDao.update(tp);
   }

   @Transactional
   public void updatePayStatus(PayChannel payChannel) {
      this.payChannelDao.update(payChannel);
      PayStage updatePs = new PayStage();
      updatePs.setId(payChannel.getStageId());
      updatePs.setPaymentCode(payChannel.getPaymentCode());
      updatePs.setChannel(payChannel.getChannel());
      updatePs.setPaidStatus(Integer.valueOf(1));
      updatePs.setPaidAt(payChannel.getPaidAt());
      this.payStageDao.update(updatePs);
      PayStage existPs = (PayStage)this.payStageDao.findById(payChannel.getStageId());
      TradePay existTp = (TradePay)this.tradePayDao.findById(existPs.getPayId());
      Integer num = this.payStageDao.countNoPay(existPs.getPayId(), Integer.valueOf(0));
      TradePay updateTp = new TradePay();
      updateTp.setId(existPs.getPayId());
      updateTp.setCurrentStage(existPs.getCurrentStage());
      updateTp.setPaidAt(payChannel.getPaidAt());
      updateTp.setPaidStatus(Integer.valueOf(num.intValue() > 0?0:1));
      updateTp.setAlreadyFee(Integer.valueOf(existTp.getAlreadyFee().intValue() + existPs.getFee().intValue()));
      this.tradePayDao.update(updateTp);
   }

   @Transactional
   public void createTradePayWithStage(TradePay tradePay, List stages) {
      this.tradePayDao.create(tradePay);

      for(PayStage payStage : stages) {
         payStage.setPayId(tradePay.getId());
         this.payStageDao.create(payStage);
      }

   }
}
