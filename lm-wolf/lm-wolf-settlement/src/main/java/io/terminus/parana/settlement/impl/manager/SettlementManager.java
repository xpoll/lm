package io.terminus.parana.settlement.impl.manager;

import io.terminus.parana.settlement.impl.dao.AfterSalesSettlementDao;
import io.terminus.parana.settlement.impl.dao.BalanceSettlementDao;
import io.terminus.parana.settlement.impl.dao.CommissionDetailDao;
import io.terminus.parana.settlement.impl.dao.DiscountDetailDao;
import io.terminus.parana.settlement.impl.dao.OrderSettlementDao;
import io.terminus.parana.settlement.model.AfterSalesSettlement;
import io.terminus.parana.settlement.model.BalanceSettlement;
import io.terminus.parana.settlement.model.CommissionDetail;
import io.terminus.parana.settlement.model.OrderSettlement;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

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
   public void createAfterSaleRefundBalanceSettlement(BalanceSettlement balanceSettlement, AfterSalesSettlement afterSalesSettlement, List commissionDetails) {
      this.balanceSettlementDao.create(balanceSettlement);
      this.afterSalesSettlementDao.create(afterSalesSettlement);

      for(CommissionDetail detail : commissionDetails) {
         detail.setOrderId(afterSalesSettlement.getId());
         this.commissionDetailDao.create(detail);
      }

   }

   @Transactional
   public void createOrderSettlement(OrderSettlement orderSettlement, List commissionDetails) {
      this.orderSettlementDao.create(orderSettlement);

      for(CommissionDetail detail : commissionDetails) {
         detail.setOrderId(orderSettlement.getOrderId());
         this.commissionDetailDao.create(detail);
      }

   }
}
