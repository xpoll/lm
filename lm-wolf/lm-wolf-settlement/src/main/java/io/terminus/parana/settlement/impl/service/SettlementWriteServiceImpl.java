package io.terminus.parana.settlement.impl.service;

import com.google.common.base.Preconditions;
import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.parana.settlement.enums.CheckStatus;
import io.terminus.parana.settlement.impl.dao.AfterSalesSettlementDao;
import io.terminus.parana.settlement.impl.dao.BalanceSettlementDao;
import io.terminus.parana.settlement.impl.dao.CommissionRuleDao;
import io.terminus.parana.settlement.impl.dao.DiscountDetailDao;
import io.terminus.parana.settlement.impl.dao.OrderSettlementDao;
import io.terminus.parana.settlement.impl.dao.PayChannelDetailDao;
import io.terminus.parana.settlement.impl.manager.SettlementManager;
import io.terminus.parana.settlement.model.AfterSalesSettlement;
import io.terminus.parana.settlement.model.BalanceSettlement;
import io.terminus.parana.settlement.model.CommissionRule;
import io.terminus.parana.settlement.model.DiscountDetail;
import io.terminus.parana.settlement.model.OrderSettlement;
import io.terminus.parana.settlement.model.PayChannelDetail;
import io.terminus.parana.settlement.service.SettlementWriteService;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SettlementWriteServiceImpl implements SettlementWriteService {
   private static final Logger log = LoggerFactory.getLogger(SettlementWriteServiceImpl.class);
   @Autowired
   private OrderSettlementDao orderSettlementDao;
   @Autowired
   private BalanceSettlementDao balanceSettlementDao;
   @Autowired
   private AfterSalesSettlementDao afterSalesSettlementDao;
   @Autowired
   private PayChannelDetailDao payChannelDetailDao;
   @Autowired
   private CommissionRuleDao commissionRuleDao;
   @Autowired
   private DiscountDetailDao discountDetailDao;
   @Autowired
   private SettlementManager settlementManager;

   public Response createOrderSettlement(OrderSettlement orderSettlement) {
      try {
         OrderSettlement exist = this.orderSettlementDao.findByOrderId(orderSettlement.getOrderId());
         if(Arguments.isNull(exist)) {
            this.orderSettlementDao.create(orderSettlement);
         }

         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to create order settlement by orderSettlement({}), caused:{}", orderSettlement, Throwables.getStackTraceAsString(var3));
         return Response.fail("create.order.settlement.fail");
      }
   }

   public Response createOrderSettlement(OrderSettlement orderSettlement, List commissionDetails) {
      try {
         OrderSettlement exist = this.orderSettlementDao.findByOrderId(orderSettlement.getOrderId());
         if(Arguments.isNull(exist)) {
            this.settlementManager.createOrderSettlement(orderSettlement, commissionDetails);
         }

         return Response.ok(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("failed to create order settlement by orderSettlement({}), caused:{}", orderSettlement, Throwables.getStackTraceAsString(var4));
         return Response.fail("create.order.settlement.fail");
      }
   }

   public Response updateOrderSettlement(OrderSettlement orderSettlement) {
      try {
         Boolean success = this.orderSettlementDao.update(orderSettlement);
         Preconditions.checkState(success.booleanValue(), "update.order.settlement.fail");
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to update order settlement by orderSettlement({}), caused:{}", orderSettlement, Throwables.getStackTraceAsString(var3));
         return Response.fail("update.order.settlement.fail");
      }
   }

   public Response createPaidBalanceSettlement(BalanceSettlement balanceSettlement) {
      try {
         this.balanceSettlementDao.create(balanceSettlement);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to create balance settlement by balanceSettlement({}), caused:{}", balanceSettlement, Throwables.getStackTraceAsString(var3));
         return Response.fail("create.balance.settlement.fail");
      }
   }

   public Response createSaleRefundBalanceSettlement(BalanceSettlement balanceSettlement) {
      try {
         Boolean success = this.balanceSettlementDao.create(balanceSettlement);
         Preconditions.checkState(success.booleanValue(), "create.balance.settlement.fail");
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to create sale balance settlement by balanceSettlement({}), caused:{}", balanceSettlement, Throwables.getStackTraceAsString(var3));
         return Response.fail("create.balance.settlement.fail");
      }
   }

   public Response createAfterSaleRefundBalanceSettlement(BalanceSettlement balanceSettlement, AfterSalesSettlement afterSalesSettlement, List commissionDetails) {
      try {
         this.settlementManager.createAfterSaleRefundBalanceSettlement(balanceSettlement, afterSalesSettlement, commissionDetails);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var5) {
         log.error("failed to create after sale balance settlement by balanceSettlement({}), caused:{}", balanceSettlement, Throwables.getStackTraceAsString(var5));
         return Response.fail("create.balance.settlement.fail");
      }
   }

   public Response updateBalanceSettlement(BalanceSettlement balanceSettlement) {
      try {
         Boolean success = this.balanceSettlementDao.update(balanceSettlement);
         Preconditions.checkState(success.booleanValue(), "update.balance.settlement.fail");
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to update balance settlement by balanceSettlement({}), caused:{}", balanceSettlement, Throwables.getStackTraceAsString(var3));
         return Response.fail("update.balance.settlement.fail");
      }
   }

   public Response createAfterSalesSettlement(AfterSalesSettlement afterSalesSettlement) {
      try {
         Boolean success = this.afterSalesSettlementDao.create(afterSalesSettlement);
         Preconditions.checkState(success.booleanValue(), "create.after.settlement.fail");
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to create after settlement by afterSalesSettlement({}), caused:{}", afterSalesSettlement, Throwables.getStackTraceAsString(var3));
         return Response.fail("create.after.settlement.fail");
      }
   }

   public Response updateAfterSalesSettlement(AfterSalesSettlement afterSalesSettlement) {
      try {
         Boolean success = this.afterSalesSettlementDao.update(afterSalesSettlement);
         Preconditions.checkState(success.booleanValue(), "update.after.settlement.fail");
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to update after settlement by afterSalesSettlement({}), caused:{}", afterSalesSettlement, Throwables.getStackTraceAsString(var3));
         return Response.fail("update.after.settlement.fail");
      }
   }

   public Response createPayChannelDetail(PayChannelDetail payChannelDetail) {
      try {
         Boolean success = this.payChannelDetailDao.create(payChannelDetail);
         Preconditions.checkState(success.booleanValue(), "create.pay.channel.detail.fail");
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to create pay channel detail by payChannelDetail({}), caused:{}", payChannelDetail, Throwables.getStackTraceAsString(var3));
         return Response.fail("create.pay.channel.detail.fail");
      }
   }

   public Response updatePayChannelDetail(PayChannelDetail payChannelDetail) {
      try {
         Boolean success = this.payChannelDetailDao.update(payChannelDetail);
         Preconditions.checkState(success.booleanValue(), "update.pay.channel.detail.fail");
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to update pay channel detail by payChannelDetail({}), caused:{}", payChannelDetail, Throwables.getStackTraceAsString(var3));
         return Response.fail("update.pay.channel.detail.fail");
      }
   }

   public Response updatePayChannelDetailStatus(Long id, CheckStatus status) {
      Response<Boolean> result = new Response();

      try {
         this.payChannelDetailDao.updateStatus(id, status);
         result.setResult(Boolean.TRUE);
      } catch (Exception var5) {
         log.error("update pay channel sum detail check status: {} fail,cause:{}", status.toString(), Throwables.getStackTraceAsString(var5));
         result.setError("update.pay.channel.sum.detail.fail");
      }

      return result;
   }

   public Response createCommissionRule(CommissionRule commissionRule) {
      Response<Boolean> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(commissionRule), "commission.rule.is.null");
         CommissionRule exist = this.commissionRuleDao.findByBusinessIdAndBusinessTypeAndCommissionType(commissionRule.getBusinessId(), commissionRule.getBusinessType(), commissionRule.getType());
         Preconditions.checkArgument(Arguments.isNull(exist), "duplicate.commission.rule");
         this.commissionRuleDao.create(commissionRule);
         result.setResult(Boolean.TRUE);
      } catch (IllegalArgumentException var4) {
         log.error("create commission rule :{} fail, error: {}", commissionRule, var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("create commission rule :{} fail, cause: {}", commissionRule, Throwables.getStackTraceAsString(var5));
         result.setError("create.commission.rule.fail");
      }

      return result;
   }

   public Response updateCommissionRate(Long commissionRuleId, Integer rate) {
      Response<Boolean> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(commissionRuleId), "commission.rule.id.invalid");
         Preconditions.checkArgument(Arguments.notNull(rate), "commission.rule.rate.invalid");
         CommissionRule exist = (CommissionRule)this.commissionRuleDao.findById(commissionRuleId);
         Preconditions.checkState(Arguments.notNull(exist), "commission.rule.not.exist");
         CommissionRule update = new CommissionRule();
         update.setId(commissionRuleId);
         update.setRate(rate);
         this.commissionRuleDao.update(update);
         result.setResult(Boolean.TRUE);
      } catch (IllegalArgumentException | IllegalStateException var6) {
         log.error("update commission rate where id: {} rate: {} fail,error:{}", new Object[]{commissionRuleId, rate, var6.getMessage()});
         result.setError(var6.getMessage());
      } catch (Exception var7) {
         log.error("update commission rate where id: {} rate: {} fail,cause:{}", new Object[]{commissionRuleId, rate, Throwables.getStackTraceAsString(var7)});
         result.setError("update.commission.fail");
      }

      return null;
   }

   public Response createDiscountDetail(DiscountDetail discountDetail) {
      Response<Boolean> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(discountDetail), "discount.detail.is.null");
         this.discountDetailDao.create(discountDetail);
      } catch (IllegalArgumentException var4) {
         log.error("crate discount detail fail,error:{}", var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("crate discount detail fail,cause:{}", Throwables.getStackTraceAsString(var5));
         result.setError("create.discount.detail.fail");
      }

      return result;
   }
}
