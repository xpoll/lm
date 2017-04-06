package cn.blmdz.wolf.settlement.impl.manager;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import cn.blmdz.wolf.settlement.impl.dao.AfterSalesSettlementDao;
import cn.blmdz.wolf.settlement.impl.dao.BalanceSettlementDao;
import cn.blmdz.wolf.settlement.impl.dao.CommissionDetailDao;
import cn.blmdz.wolf.settlement.impl.dao.DiscountDetailDao;
import cn.blmdz.wolf.settlement.impl.dao.OrderSettlementDao;
import cn.blmdz.wolf.settlement.model.AfterSalesSettlement;
import cn.blmdz.wolf.settlement.model.BalanceSettlement;
import cn.blmdz.wolf.settlement.model.CommissionDetail;
import cn.blmdz.wolf.settlement.model.OrderSettlement;

@Component
public class SettlementManager {
   private static final Logger log = LoggerFactory.getLogger(SettlementManager.class);
   @Autowired
   private BalanceSettlementDao balanceSettlementDao;
   @Autowired
   private AfterSalesSettlementDao afterSalesSettlementDao;
   @Autowired
   private CommissionDetailDao commissionDetailDao;
   @Autowired
   private DiscountDetailDao discountDetailDao;
   @Autowired
   private OrderSettlementDao orderSettlementDao;

   @Transactional
   public void createAfterSaleRefundBalanceSettlement(BalanceSettlement balanceSettlement, AfterSalesSettlement afterSalesSettlement, List<CommissionDetail> commissionDetails) {
      this.balanceSettlementDao.create(balanceSettlement);
      this.afterSalesSettlementDao.create(afterSalesSettlement);

      for(CommissionDetail detail : commissionDetails) {
         detail.setOrderId(afterSalesSettlement.getId());
         this.commissionDetailDao.create(detail);
      }

   }

   @Transactional
   public void createOrderSettlement(OrderSettlement orderSettlement, List<CommissionDetail> commissionDetails) {
      this.orderSettlementDao.create(orderSettlement);

      for(CommissionDetail detail : commissionDetails) {
         detail.setOrderId(orderSettlement.getOrderId());
         this.commissionDetailDao.create(detail);
      }

   }
}
