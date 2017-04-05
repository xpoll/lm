package io.terminus.parana.pay.impl.service;

import com.google.common.base.Function;
import com.google.common.base.Preconditions;
import com.google.common.base.Predicate;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import io.terminus.common.model.PageInfo;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.common.utils.DayRange;
import io.terminus.parana.pay.dto.ThirdPartyFeeDto;
import io.terminus.parana.pay.enums.PayChannelType;
import io.terminus.parana.pay.enums.PayInfoDto;
import io.terminus.parana.pay.impl.dao.AlipayTransDao;
import io.terminus.parana.pay.impl.dao.ExceptionPayTrackDao;
import io.terminus.parana.pay.impl.dao.OwnerPayChannelDao;
import io.terminus.parana.pay.impl.dao.PayChannelDao;
import io.terminus.parana.pay.impl.dao.PayStageDao;
import io.terminus.parana.pay.impl.dao.TradePayDao;
import io.terminus.parana.pay.impl.dao.WechatpayTransDao;
import io.terminus.parana.pay.impl.gateway.PayGateway;
import io.terminus.parana.pay.model.AlipayTrans;
import io.terminus.parana.pay.model.ExceptionPayTrack;
import io.terminus.parana.pay.model.OwnerPayChannel;
import io.terminus.parana.pay.model.PayChannel;
import io.terminus.parana.pay.model.PayStage;
import io.terminus.parana.pay.model.TradePay;
import io.terminus.parana.pay.model.WechatpayTrans;
import io.terminus.parana.pay.service.PayReadService;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class PayReadServiceImpl implements PayReadService {
   private static final Logger log = LoggerFactory.getLogger(PayReadServiceImpl.class);
   @Autowired
   private PayStageDao payStageDao;
   @Autowired
   private TradePayDao tradePayDao;
   @Autowired
   private PayChannelDao payChannelDao;
   @Autowired
   private OwnerPayChannelDao ownerPayChannelDao;
   @Autowired
   private AlipayTransDao alipayTransDao;
   @Autowired
   private WechatpayTransDao wechatpayTransDao;
   @Autowired
   private ExceptionPayTrackDao exceptionPayTrackDao;
   @Autowired
   private PayGateway payGateway;

   public Response isAllStagePaid(String tradeNo) {
      Response<Boolean> result = new Response();

      try {
         Response<TradePay> tradePayRes = this.findTradePayByTradeNo(tradeNo);
         Preconditions.checkState(tradePayRes.isSuccess(), tradePayRes.getError());
         TradePay exist = (TradePay)tradePayRes.getResult();
         if(!exist.getPaidStatus().equals(Integer.valueOf(1))) {
            result.setResult(Boolean.FALSE);
         } else {
            result.setResult(Boolean.TRUE);
         }
      } catch (Exception var5) {
         log.error("valid is all stage paid where trade no:{} fail,cause:{}", tradeNo, Throwables.getStackTraceAsString(var5));
         result.setError("check.is.all.stage.paid.fail");
      }

      return result;
   }

   public Response isMultiplePay(String tradeNo) {
      Response<Boolean> result = new Response();

      try {
         Response<PayChannel> paychannelRes = this.findPayChannelByTradeNoForPaid(tradeNo);
         Preconditions.checkState(paychannelRes.isSuccess(), paychannelRes.getError());
         PayChannel existPayChannel = (PayChannel)paychannelRes.getResult();
         List<PayChannel> payChannelList = this.payChannelDao.findByStageIdAndTypeAndStatus(existPayChannel.getStageId(), PayChannelType.PAID, Integer.valueOf(1));
         if(payChannelList.size() > 1) {
            result.setResult(Boolean.TRUE);
         } else {
            result.setResult(Boolean.FALSE);
         }
      } catch (IllegalStateException var6) {
         log.error("check is multiple pay where trade no: {} fail,error: {}", tradeNo, var6.getMessage());
         result.setError(var6.getMessage());
      } catch (Exception var7) {
         log.error("check is multiple pay where trade no: {} fail,cause: {}", tradeNo, Throwables.getStackTraceAsString(var7));
         result.setError("check.is.multiple.pay.fail");
      }

      return result;
   }

   public Response validIsExistPayChannelByStageIdAndChannel(Long stageId, String channel) {
      Response<Boolean> result = new Response();

      try {
         PayChannel payChannel = this.payChannelDao.findByStageIdAndChannel(stageId, channel, PayChannelType.PAID);
         if(Arguments.isNull(payChannel)) {
            result.setResult(Boolean.FALSE);
         } else {
            result.setResult(Boolean.TRUE);
         }
      } catch (Exception var5) {
         log.error("valid isExist pay channel isNull by payId:{} channel:{} fail cause:{}", new Object[]{stageId, channel, Throwables.getStackTraceAsString(var5)});
         result.setError("valid.pay.channel.fail");
      }

      return result;
   }

   public Response findPayStageById(Long id) {
      Response<PayStage> result = new Response();

      try {
         result.setResult(this.payStageDao.findById(id));
      } catch (Exception var4) {
         log.error("find pay stage by id:{} fail cause:{}", id, Throwables.getStackTraceAsString(var4));
         result.setError("find.pay.stage.fail");
      }

      return result;
   }

   public Response findTradePayBySystemNo(String systemNo) {
      Response<TradePay> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(systemNo), "system.no.is.null");
         TradePay tradePay = this.tradePayDao.loadBySystemNo(systemNo);
         Preconditions.checkState(Arguments.notNull(tradePay), "trade.pay.not.exist");
         result.setResult(tradePay);
      } catch (IllegalStateException var4) {
         log.error("find trade pay by systemNO:{} fail error:{}", systemNo, var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("find trade pay by systemNO:{} fail cause:{}", systemNo, Throwables.getStackTraceAsString(var5));
         result.setError("find.trade.pay.by.systemNo.fail");
      }

      return result;
   }

   public Response findTradePayById(Long payId) {
      Response<TradePay> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(payId), "trade.pay.id.invalid");
         TradePay tradePay = (TradePay)this.tradePayDao.findById(payId);
         Preconditions.checkState(Arguments.notNull(tradePay), "trade.pay.not.exist");
         result.setResult(tradePay);
      } catch (IllegalStateException var4) {
         log.error("find trade pay by id:{} fail error:{}", payId, var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("find trade pay by id:{} fail cause:{}", payId, Throwables.getStackTraceAsString(var5));
         result.setError("query.trade.pay.fail");
      }

      return result;
   }

   public Response findPayChannelById(Long id) {
      Response<PayChannel> result = new Response();

      try {
         result.setResult(this.payChannelDao.findById(id));
      } catch (Exception var4) {
         log.error("find pay channel by id: {} fail,cause: {}", id, Throwables.getStackTraceAsString(var4));
         result.setError("query.pay.channel.fail");
      }

      return result;
   }

   public Response findTradePayByTradeNo(String tradeNo) {
      Response<TradePay> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(tradeNo), "trade.no.is.null");
         PayChannel payChannel = this.payChannelDao.findByTradeNo(tradeNo, PayChannelType.PAID);
         Preconditions.checkState(Arguments.notNull(payChannel), "pay.channel.not.exist");
         PayStage payStage = (PayStage)this.payStageDao.findById(payChannel.getStageId());
         Preconditions.checkState(Arguments.notNull(payStage), "pay.stage.not.exist");
         TradePay tradePay = (TradePay)this.tradePayDao.findById(payStage.getPayId());
         Preconditions.checkState(Arguments.notNull(tradePay), "trade.pay.not.exist");
         result.setResult(tradePay);
      } catch (IllegalStateException var6) {
         log.error("find trade pay by tradeNo:{} fail error:{}", tradeNo, var6.getMessage());
         result.setError(var6.getMessage());
      } catch (Exception var7) {
         log.error("find trade pay by tradeNo:{} fail cause:{}", tradeNo, Throwables.getStackTraceAsString(var7));
         result.setError("find.trade.pay.by.tradeNo.fail");
      }

      return result;
   }

   public Response findPayChannelByTradeNoForPaid(String tradeNo) {
      Response<PayChannel> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(tradeNo), "trade.no.is.null");
         PayChannel payChannel = this.payChannelDao.findByTradeNo(tradeNo, PayChannelType.PAID);
         Preconditions.checkState(Arguments.notNull(payChannel), "pay.channel.not.exist");
         result.setResult(payChannel);
      } catch (IllegalStateException var4) {
         log.error("find pay channel by tradeNo:{} fail error:{}", tradeNo, var4.getMessage());
         result.setError(var4.getMessage());
      } catch (IllegalArgumentException var5) {
         log.error("find pay channel by tradeNo:{} fail error:{}", tradeNo, var5.getMessage());
         result.setError(var5.getMessage());
      } catch (Exception var6) {
         log.error("find pay channel by tradeNo:{} fail cause:{}", tradeNo, Throwables.getStackTraceAsString(var6));
         result.setError("find.pay.channel.by.tradeNo.fail");
      }

      return result;
   }

   public Response findPayChannelByBatchNoForRefund(String batchNo) {
      Response<PayChannel> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(batchNo), "batch.no.is.null");
         PayChannel payChannel = this.payChannelDao.findByBatchNo(batchNo);
         result.setResult(payChannel);
      } catch (IllegalStateException var4) {
         log.error("find pay channel by batchNo:{} fail error:{}", batchNo, var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("find pay channel by batchNo:{} fail cause:{}", batchNo, Throwables.getStackTraceAsString(var5));
         result.setError("find.pay.channel.by.batch.no.fail");
      }

      return result;
   }

   public Response findPayChannelByPaymentCodeForPaid(String paymentCode) {
      Response<PayChannel> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(paymentCode), "payment.no.invalid");
         PayChannel payChannel = this.payChannelDao.findByPaymentCode(paymentCode, PayChannelType.PAID);
         result.setResult(payChannel);
      } catch (IllegalStateException var4) {
         log.error("find pay channel by payment code:{} fail error:{}", paymentCode, var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("find pay channel by payment code:{} fail cause:{}", paymentCode, Throwables.getStackTraceAsString(var5));
         result.setError("find.pay.channel.fail");
      }

      return result;
   }

   public Response findPayChannelByStageIdForPaid(Long stageId) {
      Response<PayChannel> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(stageId), "stage.id.invalid");
         result.setResult(this.payChannelDao.findPaidPayChannelByStageId(stageId));
      } catch (IllegalArgumentException var4) {
         log.error("find pay channel by stage id: {} fail,error: {}", stageId, var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("find pay channel by stage id: {} fail,cause: {}", stageId, Throwables.getStackTraceAsString(var5));
         result.setError("find.pay.channel.fail");
      }

      return result;
   }

   public Response findPayChannelByStageIdAndChannel(Long stageId, String channel) {
      Response<PayChannel> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(stageId), "stage.id.invalid");
         Preconditions.checkArgument(!Strings.isNullOrEmpty(channel), "pay.channel.invalid");
         result.setResult(this.payChannelDao.findByStageIdAndChannel(stageId, channel, PayChannelType.PAID));
      } catch (IllegalArgumentException var5) {
         log.error("find pay channel by stage id: {} channel: {} fail,error: {}", new Object[]{stageId, channel, var5.getMessage()});
         result.setError(var5.getMessage());
      } catch (Exception var6) {
         log.error("find pay channel by stage id: {} channel: {} fail,cause: {}", new Object[]{stageId, channel, Throwables.getStackTraceAsString(var6)});
         result.setError("find.pay.channel.fail");
      }

      return result;
   }

   public Response findPayStagesByPayId(Long payId) {
      Response<List<PayStage>> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(payId), "pay.id.is.null");
         List<PayStage> payStageList = this.payStageDao.findByPayId(payId);
         result.setResult(payStageList);
      } catch (IllegalArgumentException var4) {
         log.error("find pay stage by payid fail error:{}", var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("find pay stage by payid:{} fail cause:{}", payId, Throwables.getStackTraceAsString(var5));
         result.setError("find.pay.stage.by.payid.fail");
      }

      return result;
   }

   public Response findPayStagesBySystemNo(String systemNo) {
      Response<List<PayStage>> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(systemNo), "system.no.invalid");
         List<PayStage> payStageList = this.payStageDao.loadBySystemNo(systemNo);
         result.setResult(payStageList);
      } catch (IllegalArgumentException var4) {
         log.error("find pay stage by systemNo fail error:{}", var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("find pay stage by systemNo:{} fail cause:{}", systemNo, Throwables.getStackTraceAsString(var5));
         result.setError("find.pay.stage.fail");
      }

      return result;
   }

   public Response findRecentStageBySystemNo(String systemNO) {
      Response<PayStage> result = new Response();

      try {
         PayStage payStage = this.payStageDao.findRecentStageByPaySystemNo(systemNO);
         Preconditions.checkState(Arguments.notNull(payStage), "pay.stage.not.exist");
         result.setResult(payStage);
      } catch (Exception var4) {
         log.error("find stage by system no:{} fail, cause:{}", systemNO, Throwables.getStackTraceAsString(var4));
         result.setError("find.recent.stage.fail");
      }

      return result;
   }

   public Response findStageByPaymentCode(String paymentCode) {
      Response<PayStage> result = new Response();

      try {
         PayStage payStage = this.payStageDao.findByPayPaymentCode(paymentCode);
         Preconditions.checkState(Arguments.notNull(payStage), "pay.stage.not.exist");
         result.setResult(payStage);
      } catch (Exception var4) {
         log.error("find stage by payment code:{} fail, cause:{}", paymentCode, Throwables.getStackTraceAsString(var4));
         result.setError("find.pay.stage.fail");
      }

      return result;
   }

   public Response checkNeedCreateTradePay(String orderIds, Boolean mergePaid, Integer orderType) {
      Response<TradePay> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(orderIds), "param.order.ids.invalid");
         Preconditions.checkArgument(!Arguments.isNull(mergePaid), "param.merge.pid.invalid");
         Preconditions.checkArgument(!Arguments.isNull(orderType), "param.order.type.invalid");
         if(mergePaid.booleanValue()) {
            TradePay exist1 = this.tradePayDao.loadByOrderIdsAndOrderType(orderIds, orderType);
            result.setResult(exist1);
         } else {
            TradePay exist2 = this.tradePayDao.loadByOrderIdAndOrderType(Long.valueOf(orderIds), orderType);
            result.setResult(exist2);
         }
      } catch (IllegalArgumentException var6) {
         log.error("check need create trade pay by orderIds: {}  mergePid: {} orderType: {} fail,error: {}", new Object[]{orderIds, mergePaid, orderType, var6.getMessage()});
         result.setError(var6.getMessage());
      } catch (Exception var7) {
         log.error("check need create trade pay by orderIds: {}  mergePid: {} orderType: {}  fail,cause: {}", new Object[]{orderIds, mergePaid, orderType, Throwables.getStackTraceAsString(var7)});
         result.setError("check.need.create.tradepay.fail");
      }

      return result;
   }

   public Response findOwnerPayChannelByOwnerIdAndType(Long ownerId, Integer type) {
      Response<OwnerPayChannel> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(ownerId), "owner.id.invalid");
         Preconditions.checkArgument(Arguments.notNull(type), "owner.type.invalid");
         OwnerPayChannel ownerPayChannel = this.ownerPayChannelDao.findByOwnerIdAndType(ownerId, type);
         result.setResult(ownerPayChannel);
      } catch (IllegalArgumentException var5) {
         log.error("find owner pay channel by owner id: {},type: {} fail,error: {}", new Object[]{ownerId, type, var5.getMessage()});
         result.setError(var5.getMessage());
      } catch (Exception var6) {
         log.error("find owner pay channel by owner id: {},type: {} fail,cause: {}", new Object[]{ownerId, type, Throwables.getStackTraceAsString(var6)});
         result.setError("find.owner.pay.channel.fail");
      }

      return result;
   }

   public Response findPayInfo(String systemNo, Integer stage) {
      Response<PayInfoDto> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(systemNo), "system.no.invalid");
         PayInfoDto dto = new PayInfoDto();
         Response<TradePay> payRes = this.findTradePayBySystemNo(systemNo);
         TradePay tradePay = (TradePay)payRes.getResult();
         Preconditions.checkState(payRes.isSuccess(), payRes.getError());
         dto.setTradePay(tradePay);
         if(Arguments.notNull(stage) && stage.equals(Integer.valueOf(0))) {
            Response<List<PayStage>> payStageRes = this.findPayStagesBySystemNo(systemNo);
            Preconditions.checkState(payStageRes.isSuccess(), payStageRes.getError());
            dto.setAllPayStages((List)payStageRes.getResult());
            List<Long> stageIds = Lists.transform((List)payStageRes.getResult(), new Function() {
               public Long apply(PayStage input) {
                  return input.getId();
               }
            });
            List<PayChannel> payChannels = this.payChannelDao.findByStageIdAndPaid(stageIds);
            dto.setAllPayChannels(payChannels);
         }

         if(Arguments.notNull(stage) && stage.intValue() > 0) {
            PayStage payStage = this.payStageDao.findByPayIdAndStage(tradePay.getId(), Integer.valueOf(Arguments.isNull(tradePay.getCurrentStage())?1:tradePay.getCurrentStage().intValue()));
            Preconditions.checkState(Arguments.notNull(payStage), "pay.stage.not.exist");
            PayChannel payChannel = this.payChannelDao.findByPaymentCode(payStage.getPaymentCode(), PayChannelType.PAID);
            Preconditions.checkState(Arguments.notNull(payChannel), "pay.channel.not.exist");
            dto.setPayStage(payStage);
            dto.setPayChannel(payChannel);
         }

         result.setResult(dto);
      } catch (IllegalArgumentException var10) {
         log.error("get pay info fail, error: {}", var10.getMessage());
         result.setError(var10.getMessage());
      } catch (IllegalStateException var11) {
         log.error("get pay info fail, cause: {}", var11.getMessage());
         result.setError(var11.getMessage());
      } catch (Exception var12) {
         log.error("get pay info fail, cause: {}", Throwables.getStackTraceAsString(var12));
         result.setError("get.pay.info.fail");
      }

      return result;
   }

   public Response pagingNeedGenerateChannelDetails(Integer pageNo, Integer size) {
      Response<Paging<PayChannel>> result = new Response();

      try {
         PageInfo pageInfo = new PageInfo(pageNo, size);
         result.setResult(this.payChannelDao.pagingNeedGenerateChannelDetails(pageInfo.getOffset(), pageInfo.getLimit()));
      } catch (Exception var5) {
         log.error("query need sum channel details fail cause:{}", Throwables.getStackTraceAsString(var5));
         result.setError("find.pay.channel.fail");
      }

      return result;
   }

   public Response findRefundByRefundIdAndChannel(Long refundOrderId, String channel) {
      Response<PayChannel> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(refundOrderId), "refund.order.id.invalid");
         Preconditions.checkArgument(!Strings.isNullOrEmpty(channel), "pay.channel.invalid");
         result.setResult(this.payChannelDao.findRefundByRefundIdAndChannel(refundOrderId, channel));
      } catch (IllegalArgumentException var5) {
         log.error("find pay channel info by refund order id: {} channel: {} fail,error: {}", new Object[]{refundOrderId, channel, var5.getMessage()});
         result.setError(var5.getMessage());
      } catch (Exception var6) {
         log.error("find pay channel info by refund order id: {} channel: {} fail,cause: {}", new Object[]{refundOrderId, channel, Throwables.getStackTraceAsString(var6)});
         result.setError("find.pay.channel.fail");
      }

      return result;
   }

   public Response pagingTimeoutPayTracks(Integer pageNo, Integer pageSize, Map criteria) {
      Response<Paging<ExceptionPayTrack>> resp = new Response();

      try {
         PageInfo page = new PageInfo(pageNo, pageSize);
         Map<String, Object> nonNullAndEmpty = this.filterNullOrEmpty(criteria);
         String startDate = null;
         String endDate = null;
         if(criteria.containsKey("startAt")) {
            startDate = criteria.get("startAt").toString();
         }

         if(criteria.containsKey("endAt")) {
            endDate = criteria.get("endAt").toString();
         }

         criteria.putAll(DayRange.of(startDate, endDate).toMap("startAt", "endAt"));
         Paging<ExceptionPayTrack> trackPaging = this.exceptionPayTrackDao.paging(page.getOffset(), page.getLimit(), nonNullAndEmpty);
         resp.setResult(trackPaging);
      } catch (Exception var10) {
         log.error("failed to paging timeout pay track(pageNo={}, pageSize={}, criteria={}), cause: {}", new Object[]{pageNo, pageSize, criteria, Throwables.getStackTraceAsString(var10)});
         resp.setError("timeout.pay.track.find.fail");
      }

      return resp;
   }

   public Response getTimeoutPayTrackById(Long id) {
      Response<ExceptionPayTrack> result = new Response();

      try {
         result.setResult(this.exceptionPayTrackDao.findById(id));
      } catch (Exception var4) {
         log.error("query timeout pay track by id:{} fail,cause:{}", id, Throwables.getStackTraceAsString(var4));
         result.setError("query.timeout.pay.track.fail");
      }

      return result;
   }

   public Response getThirdPartyFeeForward(String paymentCode, String channel) {
      Response<ThirdPartyFeeDto> result = new Response();

      try {
         ThirdPartyFeeDto thirdPartyFeeDto = this.payGateway.getThirdPartyFee(channel, paymentCode);
         result.setResult(thirdPartyFeeDto);
      } catch (Exception var5) {
         log.error("get third party fee where payment code: {},channel: {}, cause: {}", new Object[]{paymentCode, channel, Throwables.getStackTraceAsString(var5)});
         result.setError("query.third.party.fee.fail");
      }

      return result;
   }

   public Response getThirdPartyFeeReverse(String paymentCode, String channel) {
      Response<List<ThirdPartyFeeDto>> result = new Response();

      try {
         List<ThirdPartyFeeDto> dtos = this.payGateway.getThirdPartyFeeRefunds(channel, paymentCode);
         result.setResult(dtos);
      } catch (Exception var5) {
         log.error("get third party fee refund where payment code: {},channel: {}, cause: {}", new Object[]{paymentCode, channel, Throwables.getStackTraceAsString(var5)});
         result.setError("query.third.party.fee.fail");
      }

      return result;
   }

   public Response findAlipayTransByTradeNo(String tradeNo) {
      Response<List<AlipayTrans>> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(tradeNo), "trade.no.is.null");
         result.setResult(this.alipayTransDao.findByTradeNo(tradeNo));
      } catch (IllegalArgumentException var4) {
         log.error("find alipay trans by trade no={} fail error:{}", tradeNo, var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("find alipay trans by trade no={} fail cause:{}", tradeNo, Throwables.getStackTraceAsString(var5));
         result.setError("find.alipay.trans.fail");
      }

      return result;
   }

   public Response findWechatpayTransByTransactionId(String transactionId) {
      Response<List<WechatpayTrans>> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(transactionId), "transaction.id.is.null");
         result.setResult(this.wechatpayTransDao.findByTransactionId(transactionId));
      } catch (IllegalArgumentException var4) {
         log.error("find wechat pay trans by transaction id={} fail error:{}", transactionId, var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("find wechat pay trans by transaction id={} fail cause:{}", transactionId, Throwables.getStackTraceAsString(var5));
         result.setError("find.wechat.pay.trans.fail");
      }

      return result;
   }

   public Response findInnerChannelByChannel(String channel) {
      Response<String> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(channel), "pay.channel.invalid");
         result.setResult(this.payGateway.findInnerChannelByChannel(channel));
      } catch (Exception var4) {
         log.error("get inner channel by channel:{} fail,cause:{}", channel, Throwables.getStackTraceAsString(var4));
         result.setError("get.inner.channel.fail");
      }

      return result;
   }

   private Map filterNullOrEmpty(Map criteria) {
      return Maps.filterEntries(criteria, new Predicate() {
         public boolean apply(Entry entry) {
            Object v = entry.getValue();
            return v instanceof String?!Strings.isNullOrEmpty((String)v):v != null;
         }
      });
   }
}
