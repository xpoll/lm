package cn.blmdz.wolf.settlement.service;

import java.util.Date;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

import cn.blmdz.home.common.model.Response;

public interface SettlementReadService {
   Response findOrderSettlementById(@NotNull(
   message = "order.settlement.id.invalid"
) @Min(
   value = 1L,
   message = "order.settlement.id.invalid"
) Long var1);

   Response listOrderSettlements(Long var1, Long var2, Long var3, Long var4, Integer var5, Integer var6, Integer var7, Integer var8, String var9, String var10, String var11, String var12, String var13, String var14, String var15, String var16);

   Response pagingOrderSettlements(Long var1, Long var2, Long var3, Long var4, Integer var5, Integer var6, Integer var7, Integer var8, String var9, String var10, String var11, String var12, String var13, String var14, String var15, String var16, Integer var17, Integer var18);

   Response findBalanceSettlementById(@NotNull(
   message = "balance.settlement.id.invalid"
) @Min(
   value = 1L,
   message = "balance.settlement.id.invalid"
) Long var1);

   Response listBalanceSettlements(Long var1, Long var2, Long var3, Long var4, Integer var5, Integer var6, String var7, String var8, String var9, String var10, String var11, String var12, String var13, String var14, String var15);

   Response pagingBalanceSettlements(Long var1, Long var2, Long var3, Long var4, Integer var5, Integer var6, String var7, String var8, String var9, String var10, String var11, String var12, String var13, String var14, String var15, Integer var16, Integer var17);

   Response findAfterSalesSettlementById(@NotNull(
   message = "after.settlement.id.invalid"
) @Min(
   value = 1L,
   message = "after.settlement.id.invalid"
) Long var1);

   Response listAfterSalesSettlements(Long var1, Long var2, Long var3, Long var4, Long var5, Long var6, String var7, Integer var8, String var9, String var10, String var11, String var12, String var13, String var14);

   Response pagingAfterSalesSettlements(Long var1, Long var2, Long var3, Long var4, Long var5, Long var6, String var7, Integer var8, String var9, String var10, String var11, String var12, String var13, String var14, Integer var15, Integer var16);

   Response findPayChannelDetailById(@NotNull(
   message = "pay.channel.detail.id.invalid"
) @Min(
   value = 1L,
   message = "pay.channel.detail.id.invalid"
) Long var1);

   Response listPayChannelDetails(Long var1, Long var2, Long var3, String var4, String var5, String var6, String var7, String var8, Integer var9, Integer var10, String var11, String var12, String var13, String var14);

   Response pagingPayChannelDetails(Long var1, Long var2, Long var3, String var4, String var5, String var6, String var7, String var8, Integer var9, Integer var10, String var11, String var12, String var13, String var14, Integer var15, Integer var16);

   Response findPayChannelDetails(String var1, String var2, Integer var3);

   Response findPaychannelDetailNeedCheck(Integer var1, Integer var2);

   Response findMatchCommissionRule(Long var1, Integer var2, Integer var3);

   Response findBalanceSettlementByOrderIdAndType(Long var1, Integer var2);

   Response pagingSettlementByIsChecked(Integer var1, Integer var2, Boolean var3);

   Response findForwardPayChannelDetailBySystemNo(String var1);

   Response findPayChannelDetailByRefundOrderId(Long var1);

   Response findAfterSalesNeendCheck(Integer var1, Integer var2);

   Response generateSettlementSumOfShopDaily(Date var1, Date var2);

   Response generateSettlementSumOfShopDailyByPayType(Date var1, Date var2, Integer var3);

   Response generateSettlementSumOfPlatformDaily(Date var1, Date var2);

   Response generateSettlementSumOfPlatformDailyByPayType(Date var1, Date var2, Integer var3);
}
