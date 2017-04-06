package cn.blmdz.wolf.settlement.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.settlement.enums.CheckStatus;
import cn.blmdz.wolf.settlement.model.AfterSalesSettlement;
import cn.blmdz.wolf.settlement.model.BalanceSettlement;
import cn.blmdz.wolf.settlement.model.CommissionRule;
import cn.blmdz.wolf.settlement.model.DiscountDetail;
import cn.blmdz.wolf.settlement.model.OrderSettlement;
import cn.blmdz.wolf.settlement.model.PayChannelDetail;

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
