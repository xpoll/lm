package io.terminus.parana.settlement.impl.service;

import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import io.terminus.common.model.PageInfo;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.common.utils.DayRange;
import io.terminus.common.utils.Params;
import io.terminus.parana.settlement.dto.SettlementSumOfDailyDto;
import io.terminus.parana.settlement.enums.BalanceType;
import io.terminus.parana.settlement.enums.CheckStatus;
import io.terminus.parana.settlement.enums.PayChannelTransType;
import io.terminus.parana.settlement.impl.dao.AfterSalesSettlementDao;
import io.terminus.parana.settlement.impl.dao.BalanceSettlementDao;
import io.terminus.parana.settlement.impl.dao.CommissionRuleDao;
import io.terminus.parana.settlement.impl.dao.OrderSettlementDao;
import io.terminus.parana.settlement.impl.dao.PayChannelDetailDao;
import io.terminus.parana.settlement.model.AfterSalesSettlement;
import io.terminus.parana.settlement.model.BalanceSettlement;
import io.terminus.parana.settlement.model.CommissionRule;
import io.terminus.parana.settlement.model.OrderSettlement;
import io.terminus.parana.settlement.model.PayChannelDetail;
import io.terminus.parana.settlement.service.SettlementReadService;
import java.util.Date;
import java.util.List;
import java.util.Map;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SettlementReadServiceImpl implements SettlementReadService {
   private static final Logger log = LoggerFactory.getLogger(SettlementReadServiceImpl.class);
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

   public Response findOrderSettlementById(@NotNull(
   message = "order.settlement.id.invalid"
) @Min(
   value = 1L,
   message = "order.settlement.id.invalid"
) Long id) {
      try {
         OrderSettlement orderSettlement = (OrderSettlement)this.orderSettlementDao.findById(id);
         Preconditions.checkNotNull(orderSettlement, "order.settlement.not.exist");
         return Response.ok(orderSettlement);
      } catch (Exception var3) {
         log.error("failed to find order settlement by id({}), caused:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("order.settlement.not.exist");
      }
   }

   public Response listOrderSettlements(Long id, Long orderId, Long sellerId, Long shopId, Integer type, Integer payType, Integer status, Integer orderStatus, String paidStartAt, String paidEndAt, String finishStartAt, String finishEndAt, String checkedStartAt, String checkedEndAt, String startAt, String endAt) {
      try {
         Map<String, Object> criteria = Maps.newHashMap();
         criteria.put("id", id);
         criteria.put("orderId", orderId);
         criteria.put("sellerId", sellerId);
         criteria.put("shopId", shopId);
         criteria.put("type", type);
         criteria.put("payType", payType);
         criteria.put("status", status);
         criteria.put("orderStatus", orderStatus);
         criteria.putAll(DayRange.from(paidStartAt, paidEndAt).toMap("paidStartAt", "paidEndAt"));
         criteria.putAll(DayRange.from(finishStartAt, finishEndAt).toMap("finishStartAt", "finishEndAt"));
         criteria.putAll(DayRange.from(checkedStartAt, checkedEndAt).toMap("checkedStartAt", "checkedEndAt"));
         criteria.putAll(DayRange.from(startAt, endAt).toMap("startAt", "endAt"));
         List<OrderSettlement> orderSettlements = this.orderSettlementDao.list(Params.filterNullOrEmpty(criteria));
         return Response.ok(orderSettlements);
      } catch (Exception var19) {
         log.error("failed to list order settlement by id({}) orderId({}) sellerId({}) shopId({}) type({}) payType({}) orderStatus({}) paidStartAt({}) paidEndAt({}) finishStartAt({}) finishEndAt({}) checkedStartAt({}) checkedEndAt({})  startAt({}) endAt({}), caused:{}", new Object[]{id, orderId, sellerId, shopId, type, payType, orderStatus, paidStartAt, paidEndAt, finishStartAt, finishEndAt, checkedStartAt, checkedEndAt, startAt, endAt, Throwables.getStackTraceAsString(var19)});
         return Response.fail("list.order.settlement.fail");
      }
   }

   public Response pagingOrderSettlements(Long id, Long orderId, Long sellerId, Long shopId, Integer type, Integer payType, Integer status, Integer orderStatus, String paidStartAt, String paidEndAt, String checkedStartAt, String checkedEndAt, String finishStartAt, String finishEndAt, String startAt, String endAt, Integer pageNo, Integer size) {
      try {
         PageInfo page = new PageInfo(pageNo, size);
         Map<String, Object> criteria = Maps.newHashMap();
         criteria.put("id", id);
         criteria.put("orderId", orderId);
         criteria.put("sellerId", sellerId);
         criteria.put("shopId", shopId);
         criteria.put("type", type);
         criteria.put("payType", payType);
         criteria.put("status", status);
         criteria.put("orderStatus", orderStatus);
         criteria.putAll(DayRange.from(paidStartAt, paidEndAt).toMap("paidStartAt", "paidEndAt"));
         criteria.putAll(DayRange.from(finishStartAt, finishEndAt).toMap("finishStartAt", "finishEndAt"));
         criteria.putAll(DayRange.from(checkedStartAt, checkedEndAt).toMap("checkedStartAt", "checkedEndAt"));
         criteria.putAll(DayRange.from(startAt, endAt).toMap("startAt", "endAt"));
         Paging<OrderSettlement> paging = this.orderSettlementDao.paging(page.getOffset(), page.getLimit(), Params.filterNullOrEmpty(criteria));
         return Response.ok(paging);
      } catch (Exception var22) {
         log.error("failed to paging order settlement by id({}) orderId({}) sellerId({}) shopId({}) type({}) payType({}) orderStatus({}) paidStartAt({}) paidEndAt({})  finishStartAt({}) finishEndAt({}) checkedStartAt({}) checkedEndAt({}) startAt({}) endAt({}) pageNo({}) size({}), caused:{}", new Object[]{id, orderId, sellerId, shopId, type, payType, orderStatus, paidStartAt, paidEndAt, finishStartAt, finishEndAt, checkedStartAt, checkedEndAt, startAt, endAt, pageNo, size, Throwables.getStackTraceAsString(var22)});
         return Response.fail("paging.order.settlement.fail");
      }
   }

   public Response findBalanceSettlementById(@NotNull(
   message = "balance.settlement.id.invalid"
) @Min(
   value = 1L,
   message = "balance.settlement.id.invalid"
) Long id) {
      try {
         BalanceSettlement balanceSettlement = (BalanceSettlement)this.balanceSettlementDao.findById(id);
         Preconditions.checkNotNull(balanceSettlement, "balance.settlement.not.exist");
         return Response.ok(balanceSettlement);
      } catch (Exception var3) {
         log.error("failed to find balance settlement by id({}), caused:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("balance.settlement.not.exist");
      }
   }

   public Response listBalanceSettlements(Long id, Long orderId, Long sellerId, Long shopId, Integer type, Integer payType, String channel, String systemNo, String tradeNo, String paymentCode, String batchNo, String tradeStartAt, String tradeEndAt, String startAt, String endAt) {
      try {
         Map<String, Object> criteria = Maps.newHashMap();
         criteria.put("id", id);
         criteria.put("orderId", orderId);
         criteria.put("sellerId", sellerId);
         criteria.put("shopId", shopId);
         criteria.put("type", type);
         criteria.put("payType", payType);
         criteria.put("channel", channel);
         criteria.put("systemNo", systemNo);
         criteria.put("tradeNo", tradeNo);
         criteria.put("paymentCode", paymentCode);
         criteria.put("batchNo", batchNo);
         criteria.putAll(DayRange.from(tradeStartAt, tradeEndAt).toMap("tradeStartAt", "tradeEndAt"));
         criteria.putAll(DayRange.from(startAt, endAt).toMap("startAt", "endAt"));
         List<BalanceSettlement> balanceSettlements = this.balanceSettlementDao.list(Params.filterNullOrEmpty(criteria));
         return Response.ok(balanceSettlements);
      } catch (Exception var18) {
         log.error("failed to list balance settlement by id({}) orderId({}) sellerId({}) shopId({}) type({}) payType({}) channel({}) systemNo({}) tradeNo({}) paymentCode({}) batchNo({}) tradeStartAt({}) tradeEndAt({})  startAt({}) endAt({}), caused:{}", new Object[]{id, orderId, sellerId, shopId, type, payType, channel, systemNo, tradeNo, paymentCode, batchNo, tradeStartAt, tradeEndAt, startAt, endAt, Throwables.getStackTraceAsString(var18)});
         return Response.fail("list.balance.settlement.fail");
      }
   }

   public Response pagingBalanceSettlements(Long id, Long orderId, Long sellerId, Long shopId, Integer type, Integer payType, String channel, String systemNo, String tradeNo, String paymentCode, String batchNo, String tradeStartAt, String tradeEndAt, String startAt, String endAt, Integer pageNo, Integer size) {
      try {
         PageInfo page = new PageInfo(pageNo, size);
         Map<String, Object> criteria = Maps.newHashMap();
         criteria.put("id", id);
         criteria.put("orderId", orderId);
         criteria.put("sellerId", sellerId);
         criteria.put("shopId", shopId);
         criteria.put("type", type);
         criteria.put("payType", payType);
         criteria.put("channel", channel);
         criteria.put("systemNo", systemNo);
         criteria.put("tradeNo", tradeNo);
         criteria.put("paymentCode", paymentCode);
         criteria.put("batchNo", batchNo);
         criteria.putAll(DayRange.from(tradeStartAt, tradeEndAt).toMap("tradeStartAt", "tradeEndAt"));
         criteria.putAll(DayRange.from(startAt, endAt).toMap("startAt", "endAt"));
         Paging<BalanceSettlement> paging = this.balanceSettlementDao.paging(page.getOffset(), page.getLimit(), Params.filterNullOrEmpty(criteria));
         return Response.ok(paging);
      } catch (Exception var21) {
         log.error("failed to list balance settlement by id({}) orderId({}) sellerId({}) shopId({}) type({}) payType({}) channel({}) systemNo({}) tradeNo({}) paymentCode({}) batchNo({}) tradeStartAt({}) tradeEndAt({})  startAt({}) endAt({}) pageNo({}) size({}), caused:{}", new Object[]{id, orderId, sellerId, shopId, type, payType, channel, systemNo, tradeNo, paymentCode, batchNo, tradeStartAt, tradeEndAt, startAt, endAt, pageNo, size, Throwables.getStackTraceAsString(var21)});
         return Response.fail("list.balance.settlement.fail");
      }
   }

   public Response findAfterSalesSettlementById(@NotNull(
   message = "after.settlement.id.invalid"
) @Min(
   value = 1L,
   message = "after.settlement.id.invalid"
) Long id) {
      try {
         AfterSalesSettlement afterSalesSettlement = (AfterSalesSettlement)this.afterSalesSettlementDao.findById(id);
         Preconditions.checkNotNull(afterSalesSettlement, "after.settlement.not.exist");
         return Response.ok(afterSalesSettlement);
      } catch (Exception var3) {
         log.error("failed to find after settlement by id({}), caused:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("after.settlement.not.exist");
      }
   }

   public Response listAfterSalesSettlements(Long id, Long orderId, Long orderItemId, Long refundOrderId, Long sellerId, Long shopId, String channel, Integer status, String refundStartAt, String refundEndAt, String checkedStartAt, String checkedEndAt, String startAt, String endAt) {
      try {
         Map<String, Object> criteria = Maps.newHashMap();
         criteria.put("id", id);
         criteria.put("orderId", orderId);
         criteria.put("orderItemId", orderItemId);
         criteria.put("refundOrderId", refundOrderId);
         criteria.put("sellerId", sellerId);
         criteria.put("shopId", shopId);
         criteria.put("channel", channel);
         criteria.put("status", status);
         criteria.putAll(DayRange.from(refundStartAt, refundEndAt).toMap("refundStartAt", "refundEndAt"));
         criteria.putAll(DayRange.from(checkedStartAt, checkedEndAt).toMap("checkedStartAt", "checkedEndAt"));
         criteria.putAll(DayRange.from(startAt, endAt).toMap("startAt", "endAt"));
         List<AfterSalesSettlement> afterSalesSettlements = this.afterSalesSettlementDao.list(Params.filterNullOrEmpty(criteria));
         return Response.ok(afterSalesSettlements);
      } catch (Exception var17) {
         log.error("failed to list after settlement by id({}) orderId({}) orderItemId({}) refundOrderId({}) sellerId({}) shopId({}) channel({}) status({}) refundStartAt({}) refundEndAt({}) checkedStartAt({}) checkedEndAt({}) startAt({}) endAt({}), caused:{}", new Object[]{id, orderId, orderItemId, refundOrderId, sellerId, shopId, channel, status, refundStartAt, refundEndAt, checkedStartAt, checkedEndAt, startAt, endAt, Throwables.getStackTraceAsString(var17)});
         return Response.fail("list.after.settlement.fail");
      }
   }

   public Response pagingAfterSalesSettlements(Long id, Long orderId, Long orderItemId, Long refundOrderId, Long sellerId, Long shopId, String channel, Integer status, String refundStartAt, String refundEndAt, String checkedStartAt, String checkedEndAt, String startAt, String endAt, Integer pageNo, Integer size) {
      try {
         PageInfo page = new PageInfo(pageNo, size);
         Map<String, Object> criteria = Maps.newHashMap();
         criteria.put("id", id);
         criteria.put("orderId", orderId);
         criteria.put("orderItemId", orderItemId);
         criteria.put("refundOrderId", refundOrderId);
         criteria.put("sellerId", sellerId);
         criteria.put("shopId", shopId);
         criteria.put("channel", channel);
         criteria.put("status", status);
         criteria.putAll(DayRange.from(refundStartAt, refundEndAt).toMap("refundStartAt", "refundEndAt"));
         criteria.putAll(DayRange.from(checkedStartAt, checkedEndAt).toMap("checkedStartAt", "checkedEndAt"));
         criteria.putAll(DayRange.from(startAt, endAt).toMap("startAt", "endAt"));
         Paging<AfterSalesSettlement> paging = this.afterSalesSettlementDao.paging(page.getOffset(), page.getLimit(), Params.filterNullOrEmpty(criteria));
         return Response.ok(paging);
      } catch (Exception var20) {
         log.error("failed to paging after settlement by id({}) orderId({}) orderItemId({}) refundOrderId({}) sellerId({}) shopId({}) channel({}) status({}) refundStartAt({}) refundEndAt({}) checkedStartAt({}) checkedEndAt({}) startAt({}) endAt({}) pageNo({}) size({}), caused:{}", new Object[]{id, orderId, orderItemId, refundOrderId, sellerId, shopId, channel, status, checkedStartAt, checkedEndAt, refundStartAt, refundEndAt, startAt, endAt, pageNo, size, Throwables.getStackTraceAsString(var20)});
         return Response.fail("paging.after.settlement.fail");
      }
   }

   public Response findPayChannelDetailById(@NotNull(
   message = "pay.channel.detail.id.invalid"
) @Min(
   value = 1L,
   message = "pay.channel.detail.id.invalid"
) Long id) {
      try {
         PayChannelDetail payChannelDetail = (PayChannelDetail)this.payChannelDetailDao.findById(id);
         Preconditions.checkNotNull(payChannelDetail, "pay.channel.detail.not.exist");
         return Response.ok(payChannelDetail);
      } catch (Exception var3) {
         log.error("failed to find pay channel settlement by id({}), caused:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("pay.channel.detail.not.exist");
      }
   }

   public Response listPayChannelDetails(Long id, Long orderId, Long refundOrderId, String channel, String systemNo, String tradeNo, String paymentCode, String batchNo, Integer status, Integer type, String checkedStartAt, String checkedEndAt, String startAt, String endAt) {
      try {
         Map<String, Object> criteria = Maps.newHashMap();
         criteria.put("id", id);
         criteria.put("orderId", orderId);
         criteria.put("refundOrderId", refundOrderId);
         criteria.put("channel", channel);
         criteria.put("systemNo", systemNo);
         criteria.put("tradeNo", tradeNo);
         criteria.put("paymentCode", paymentCode);
         criteria.put("batchNo", batchNo);
         criteria.put("status", status);
         criteria.put("type", type);
         criteria.putAll(DayRange.from(checkedStartAt, checkedEndAt).toMap("checkedStartAt", "checkedEndAt"));
         criteria.putAll(DayRange.from(startAt, endAt).toMap("startAt", "endAt"));
         List<PayChannelDetail> payChannelDetails = this.payChannelDetailDao.list(Params.filterNullOrEmpty(criteria));
         return Response.ok(payChannelDetails);
      } catch (Exception var17) {
         log.error("failed to list balance settlement by id({}) orderId({}) refundOrderId({}) channel({}) systemNo({}) tradeNo({}) paymentCode({}) batchNo({}) status({}) type({})  checkedStartAt({}) checkedEndAt({}) startAt({}) endAt({}), caused:{}", new Object[]{id, orderId, refundOrderId, channel, systemNo, tradeNo, paymentCode, batchNo, status, type, checkedStartAt, checkedEndAt, startAt, endAt, Throwables.getStackTraceAsString(var17)});
         return Response.fail("list.pay.channel.detail.fail");
      }
   }

   public Response pagingPayChannelDetails(Long id, Long orderId, Long refundOrderId, String channel, String systemNo, String tradeNo, String paymentCode, String batchNo, Integer status, Integer type, String checkedStartAt, String checkedEndAt, String startAt, String endAt, Integer pageNo, Integer size) {
      try {
         PageInfo page = new PageInfo(pageNo, size);
         Map<String, Object> criteria = Maps.newHashMap();
         criteria.put("id", id);
         criteria.put("orderId", orderId);
         criteria.put("refundOrderId", refundOrderId);
         criteria.put("channel", channel);
         criteria.put("systemNo", systemNo);
         criteria.put("tradeNo", tradeNo);
         criteria.put("paymentCode", paymentCode);
         criteria.put("batchNo", batchNo);
         criteria.put("status", status);
         criteria.put("type", type);
         criteria.putAll(DayRange.from(checkedStartAt, checkedEndAt).toMap("checkedStartAt", "checkedEndAt"));
         criteria.putAll(DayRange.from(startAt, endAt).toMap("startAt", "endAt"));
         Paging<PayChannelDetail> paging = this.payChannelDetailDao.paging(page.getOffset(), page.getLimit(), Params.filterNullOrEmpty(criteria));
         return Response.ok(paging);
      } catch (Exception var20) {
         log.error("failed to paging pay channel detail by id({}) orderId({}) refundOrderId({}) channel({}) systemNo({}) tradeNo({}) paymentCode({}) batchNo({}) status({}) type({}) checkedStartAt({}) checkedEndAt({}) startAt({}) endAt({}) pageNo({}) size({}), caused:{}", new Object[]{id, orderId, refundOrderId, channel, systemNo, tradeNo, paymentCode, batchNo, status, type, checkedStartAt, checkedEndAt, startAt, endAt, pageNo, size, Throwables.getStackTraceAsString(var20)});
         return Response.fail("paging.pay.channel.detail.fail");
      }
   }

   public Response findPayChannelDetails(String channel, String code, Integer type) {
      Response<PayChannelDetail> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(channel), "pay.channel.invalid");
         Preconditions.checkArgument(!Strings.isNullOrEmpty(code), "code.invalid");
         if(type.equals(Integer.valueOf(1))) {
            result.setResult(this.payChannelDetailDao.findPayChannelDetailByChannelAndPaymentCode(channel, code));
         }

         if(type.equals(Integer.valueOf(2))) {
            result.setResult(this.payChannelDetailDao.findPayChannelDetailByChannelAndBatchNo(channel, code));
         }
      } catch (IllegalArgumentException var6) {
         log.error("find pay channel sum detail by channel: {} code: {} fail,error: {}", new Object[]{channel, code, var6.getMessage()});
         result.setError(var6.getMessage());
      } catch (Exception var7) {
         log.error("find pay channel sum detail by channel: {} code: {}fail,cause: {}", new Object[]{channel, code, Throwables.getStackTraceAsString(var7)});
         result.setError("query.pay.channel.sum.detail.fail");
      }

      return result;
   }

   public Response findPaychannelDetailNeedCheck(Integer pageNo, Integer size) {
      Response<Paging<PayChannelDetail>> result = new Response();

      try {
         PageInfo pageInfo = new PageInfo(pageNo, size);
         result.setResult(this.payChannelDetailDao.pagingNeedCheck(pageInfo.getOffset(), pageInfo.getLimit()));
      } catch (Exception var5) {
         log.error("query need check pay channel sum detail fail cause:{}", Throwables.getStackTraceAsString(var5));
         result.setError("pay.channel.sum.detail.find.fail");
      }

      return result;
   }

   public Response findMatchCommissionRule(Long businessId, Integer businessType, Integer commissionType) {
      Response<CommissionRule> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(businessId), "param.business.id.invalid");
         Preconditions.checkArgument(Arguments.notNull(businessType), "param.business.type.invalid");
         Preconditions.checkArgument(Arguments.notNull(commissionType), "param.commission.type.invalid");
         result.setResult(this.commissionRuleDao.findByBusinessIdAndBusinessTypeAndCommissionType(businessId, businessType, commissionType));
      } catch (Exception var6) {
         log.error("find match commission rule where businessId: {} businessType: {} commissionType: {} fail,cause: {}", new Object[]{businessId, businessType, commissionType, Throwables.getStackTraceAsString(var6)});
         result.setError("fian.match.commission.rule.fail");
      }

      return result;
   }

   public Response findBalanceSettlementByOrderIdAndType(Long orderId, Integer type) {
      Response<BalanceSettlement> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(orderId), "order.id.invalid");
         Preconditions.checkArgument(Arguments.notNull(type), "balance.type.is.null");
         BalanceType balanceType = BalanceType.from(type.intValue());
         Preconditions.checkArgument(Arguments.notNull(balanceType), "balance.type.invalid");
         result.setResult(this.balanceSettlementDao.findByOrderIdAndType(orderId, balanceType));
      } catch (IllegalArgumentException var5) {
         log.error("find balance settlement by order id: {} type: {} fail,error: {}", new Object[]{orderId, type.toString(), var5.getMessage()});
         result.setError(var5.getMessage());
      } catch (Exception var6) {
         log.error("find balance settlement by order id: {} type: {} fail,cause: {}", new Object[]{orderId, type.toString(), Throwables.getStackTraceAsString(var6)});
         result.setError("find.balance.settlement.fail");
      }

      return result;
   }

   public Response pagingSettlementByIsChecked(Integer pageNo, Integer size, Boolean isChecked) {
      Response<Paging<OrderSettlement>> result = new Response();

      try {
         PageInfo pageInfo = new PageInfo(pageNo, size);
         result.setResult(this.orderSettlementDao.pagingByIsChecked(pageInfo.getOffset(), pageInfo.getLimit(), isChecked));
      } catch (Exception var6) {
         log.error("query settlement by status fail cause:{}", Throwables.getStackTraceAsString(var6));
         result.setError("query.settlement.fail");
      }

      return result;
   }

   public Response findForwardPayChannelDetailBySystemNo(String systemNo) {
      Response<List<PayChannelDetail>> result = new Response();
      List<CheckStatus> status = Lists.newArrayList();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(systemNo), "system.no.invalid");
         status.add(CheckStatus.CHECK_SUCCESS);
         result.setResult(this.payChannelDetailDao.getPcsdBySystemNoAndStatusAndType(systemNo, status, PayChannelTransType.PAID));
      } catch (IllegalArgumentException var5) {
         log.error("query pay channel detail by system no:{},status:{} fail,error:{}", new Object[]{systemNo, status.toString(), var5.getMessage()});
         result.setError(var5.getMessage());
      } catch (Exception var6) {
         log.error("query pay channel detail by system no:{},status:{} fail,cause:{}", new Object[]{systemNo, status.toString(), Throwables.getStackTraceAsString(var6)});
         result.setError("pay.channel.daily.find.fail");
      }

      return result;
   }

   public Response findPayChannelDetailByRefundOrderId(Long refundOrderId) {
      Response<PayChannelDetail> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(refundOrderId), "refund.order.id.invalid");
         result.setResult(this.payChannelDetailDao.findByRefundOrderId(refundOrderId));
      } catch (IllegalArgumentException var4) {
         log.error("query pay channel detail by refund order id: {} fail,error:{}", refundOrderId, var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("query pay channel  detail by refund order id: {} fail,cause:{}", refundOrderId, Throwables.getStackTraceAsString(var5));
         result.setError("query.channel.detail.fail");
      }

      return result;
   }

   public Response findAfterSalesNeendCheck(Integer pageNo, Integer size) {
      Response<Paging<AfterSalesSettlement>> result = new Response();

      try {
         PageInfo pageInfo = new PageInfo(pageNo, size);
         result.setResult(this.afterSalesSettlementDao.pagingNeedCheck(pageInfo.getOffset(), pageInfo.getLimit()));
      } catch (Exception var5) {
         log.error("query need check after sales settlement fail cause:{}", Throwables.getStackTraceAsString(var5));
         result.setError("query.after.sales.settlement.fail");
      }

      return result;
   }

   public Response generateSettlementSumOfShopDaily(Date startAt, Date endAt) {
      Response<List<SettlementSumOfDailyDto>> result = new Response();

      try {
         result.setResult(this.orderSettlementDao.generateSettlementSumOfShopDaily(startAt, endAt));
      } catch (Exception var5) {
         log.error("generate settlement sum of daily shop fail cause:{}", Throwables.getStackTraceAsString(var5));
         result.setError("generate.settlement.sum.shop.daily.fail");
      }

      return result;
   }

   public Response generateSettlementSumOfShopDailyByPayType(Date startAt, Date endAt, Integer payType) {
      Response<List<SettlementSumOfDailyDto>> result = new Response();

      try {
         result.setResult(this.orderSettlementDao.generateSettlementSumOfShopDailyByPayType(startAt, endAt, payType));
      } catch (Exception var6) {
         log.error("generate settlement sum of daily shop fail cause:{}", Throwables.getStackTraceAsString(var6));
         result.setError("generate.settlement.sum.shop.daily.fail");
      }

      return result;
   }

   public Response generateSettlementSumOfPlatformDaily(Date startAt, Date endAt) {
      Response<SettlementSumOfDailyDto> result = new Response();

      try {
         result.setResult(this.orderSettlementDao.generateSettlementSumOfPlatformDaily(startAt, endAt));
      } catch (Exception var5) {
         log.error("generate settlement sum of daily platform fail cause:{}", Throwables.getStackTraceAsString(var5));
         result.setError("generate.settlement.sum.platform.daily.fail");
      }

      return result;
   }

   public Response generateSettlementSumOfPlatformDailyByPayType(Date startAt, Date endAt, Integer payType) {
      Response<SettlementSumOfDailyDto> result = new Response();

      try {
         result.setResult(this.orderSettlementDao.generateSettlementSumOfPlatformDailyByPayType(startAt, endAt, payType));
      } catch (Exception var6) {
         log.error("generate settlement sum of daily platform fail cause:{}", Throwables.getStackTraceAsString(var6));
         result.setError("generate.settlement.sum.platform.daily.fail");
      }

      return result;
   }
}
