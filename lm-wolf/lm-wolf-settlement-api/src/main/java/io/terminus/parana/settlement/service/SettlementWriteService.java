package io.terminus.parana.settlement.service;

import io.terminus.common.model.Response;
import io.terminus.parana.settlement.enums.CheckStatus;
import io.terminus.parana.settlement.model.AfterSalesSettlement;
import io.terminus.parana.settlement.model.BalanceSettlement;
import io.terminus.parana.settlement.model.CommissionRule;
import io.terminus.parana.settlement.model.DiscountDetail;
import io.terminus.parana.settlement.model.OrderSettlement;
import io.terminus.parana.settlement.model.PayChannelDetail;
import java.util.List;

public interface SettlementWriteService {
   Response createOrderSettlement(OrderSettlement var1);

   Response createOrderSettlement(OrderSettlement var1, List var2);

   Response updateOrderSettlement(OrderSettlement var1);

   Response createPaidBalanceSettlement(BalanceSettlement var1);

   Response createSaleRefundBalanceSettlement(BalanceSettlement var1);

   Response createAfterSaleRefundBalanceSettlement(BalanceSettlement var1, AfterSalesSettlement var2, List var3);

   Response updateBalanceSettlement(BalanceSettlement var1);

   Response createAfterSalesSettlement(AfterSalesSettlement var1);

   Response updateAfterSalesSettlement(AfterSalesSettlement var1);

   Response createPayChannelDetail(PayChannelDetail var1);

   Response updatePayChannelDetail(PayChannelDetail var1);

   Response updatePayChannelDetailStatus(Long var1, CheckStatus var2);

   Response createCommissionRule(CommissionRule var1);

   Response updateCommissionRate(Long var1, Integer var2);

   Response createDiscountDetail(DiscountDetail var1);
}
