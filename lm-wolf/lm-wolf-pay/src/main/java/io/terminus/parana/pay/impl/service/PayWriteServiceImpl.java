package io.terminus.parana.pay.impl.service;

import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.parana.pay.dto.PayStageInfo;
import io.terminus.parana.pay.enums.ExceptionPayType;
import io.terminus.parana.pay.enums.PayChannelStatus;
import io.terminus.parana.pay.enums.PayChannelType;
import io.terminus.parana.pay.enums.TrackHandleStatus;
import io.terminus.parana.pay.impl.dao.AlipayTransDao;
import io.terminus.parana.pay.impl.dao.ExceptionPayTrackDao;
import io.terminus.parana.pay.impl.dao.KjtpayTransDao;
import io.terminus.parana.pay.impl.dao.OwnerPayChannelDao;
import io.terminus.parana.pay.impl.dao.PayChannelDao;
import io.terminus.parana.pay.impl.dao.PayStageDao;
import io.terminus.parana.pay.impl.dao.TradePayDao;
import io.terminus.parana.pay.impl.dao.UnionPayTransDao;
import io.terminus.parana.pay.impl.dao.WechatpayTransDao;
import io.terminus.parana.pay.impl.manager.PayManager;
import io.terminus.parana.pay.model.AlipayTrans;
import io.terminus.parana.pay.model.ExceptionPayTrack;
import io.terminus.parana.pay.model.KjtpayTrans;
import io.terminus.parana.pay.model.OwnerPayChannel;
import io.terminus.parana.pay.model.PayChannel;
import io.terminus.parana.pay.model.PayStage;
import io.terminus.parana.pay.model.TradePay;
import io.terminus.parana.pay.model.UnionPayTrans;
import io.terminus.parana.pay.model.WechatpayTrans;
import io.terminus.parana.pay.service.PayWriteService;
import java.util.Date;
import java.util.List;
import java.util.Map;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class PayWriteServiceImpl implements PayWriteService {
   private static final Logger log = LoggerFactory.getLogger(PayWriteServiceImpl.class);
   @Autowired
   private TradePayDao tradePayDao;
   @Autowired
   private PayStageDao payStageDao;
   @Autowired
   private PayChannelDao payChannelDao;
   @Autowired
   private AlipayTransDao alipayTransDao;
   @Autowired
   private UnionPayTransDao unionPayTransDao;
   @Autowired
   private KjtpayTransDao kjtpayTransDao;
   @Autowired
   private WechatpayTransDao wechatpayTransDao;
   @Autowired
   private PayManager payManager;
   @Autowired
   private ExceptionPayTrackDao exceptionPayTrackDao;
   @Autowired
   private OwnerPayChannelDao ownerPayChannelDao;
   private DateTimeFormatter STD_FMT = DateTimeFormat.forPattern("YYYY-MM-dd HH:mm:ss");

   public Response updateStageFee(String systemNo, Map stageFees) {
      Response<Boolean> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(systemNo), "system.no.is.null");
         Preconditions.checkArgument(Arguments.notNull(stageFees) && stageFees.size() > 0, "param.illgeal");
         TradePay tradePay = this.tradePayDao.loadBySystemNo(systemNo);
         Preconditions.checkState(Arguments.notNull(tradePay), "trade.pay.not.exist");
         List<Integer> stages = Lists.newArrayList(stageFees.keySet());
         PayStage paidStage = this.payStageDao.checkIsPaid(tradePay.getId(), stages);
         Preconditions.checkState(Arguments.isNull(paidStage), "exist.paid.stage");
         List<PayStage> payStageList = Lists.newArrayList();

         for(Integer stage : stageFees.keySet()) {
            PayStage payStage = this.payStageDao.findByPayIdAndStage(tradePay.getId(), stage);
            Preconditions.checkState(Arguments.notNull(payStage), "pay.stage.not.exist");
            PayStage ps = new PayStage();
            ps.setId(payStage.getId());
            ps.setFee((Integer)stageFees.get(stage));
            payStageList.add(ps);
         }

         this.payManager.updateStageFee(tradePay, payStageList);
         result.setResult(Boolean.TRUE);
      } catch (IllegalArgumentException var12) {
         log.error("update stage fee fail error:{}", var12.getMessage());
         result.setError(var12.getMessage());
      } catch (IllegalStateException var13) {
         log.error("update stage fee fail error:{}", var13.getMessage());
         result.setError(var13.getMessage());
      } catch (Exception var14) {
         log.error("update stage fee fail cause:{}", Throwables.getStackTraceAsString(var14));
         result.setError("update.pay.stage.fee.fail");
      }

      return result;
   }

   public Response updatePayStage(PayStage payStage) {
      Response<Boolean> result = new Response();

      try {
         Preconditions.checkNotNull(Boolean.valueOf(Arguments.notNull(payStage)), "pay.stage.is.null");
         this.payStageDao.update(payStage);
         result.setResult(Boolean.TRUE);
      } catch (NullPointerException var4) {
         log.error("update pay stage fail error:{}", var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("update pay stage fail cause:{}", Throwables.getStackTraceAsString(var5));
         result.setError("update.pay.stage.fail");
      }

      return result;
   }

   public Response createTradePay(TradePay tradePay) {
      Response<Boolean> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(tradePay), "trade.pay.object.is.null");
         this.tradePayDao.create(tradePay);
         result.setResult(Boolean.TRUE);
      } catch (IllegalArgumentException var4) {
         log.error("create trade pay fail error:{}", var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("create trade pay fail cause:{}", Throwables.getStackTraceAsString(var5));
         result.setError("create.trade.pay.fail");
      }

      return result;
   }

   public Response updateTradePay(TradePay tradePay) {
      Response<Boolean> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(tradePay), "trade.pay.is.null");
         this.tradePayDao.update(tradePay);
         result.setResult(Boolean.TRUE);
      } catch (Exception var4) {
         result.setError("update.trade.pay.fail");
         log.error("update tradepay id:{} fail,cause:{} ", tradePay.getId(), Throwables.getCausalChain(var4));
      }

      return result;
   }

   public Response createTradePayWithStage(TradePay tradePay, List stageInfos, Date expirePayTime) {
      Response<Boolean> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(tradePay), "trade.pay.object.is.null");
         Preconditions.checkArgument(!Strings.isNullOrEmpty(tradePay.getSystemNo()), "system.no.is.null");
         Preconditions.checkArgument(Arguments.notNull(stageInfos), "stage.fee.map.is.null");
         Preconditions.checkArgument(stageInfos.size() > 0, "stage.fee.map.is.null");
         TradePay exit = this.tradePayDao.loadBySystemNo(tradePay.getSystemNo());
         if(Arguments.notNull(exit)) {
            result.setResult(Boolean.TRUE);
            return result;
         }

         List<PayStage> stages = Lists.newArrayList();

         for(PayStageInfo stage : stageInfos) {
            PayStage payStage = new PayStage();
            payStage.setPaidStatus(Integer.valueOf(0));
            payStage.setPayId(tradePay.getId());
            payStage.setFee(stage.getFee());
            payStage.setCurrentStage(stage.getCurrentStage());
            payStage.setContent("第" + stage.getCurrentStage() + "期付款");
            payStage.setRepayAt(stage.getRepayAt());
            payStage.setSystemNo(tradePay.getSystemNo());
            payStage.setCreatedAt(new Date());
            payStage.setUpdatedAt(new Date());
            payStage.setExpiredAt(expirePayTime);
            stages.add(payStage);
         }

         this.payManager.createTradePayWithStage(tradePay, stages);
         result.setResult(Boolean.TRUE);
      } catch (IllegalArgumentException var10) {
         log.error("create trade pay fail error:{}", var10.getMessage());
         result.setError(var10.getMessage());
      } catch (Exception var11) {
         log.error("create trade pay fail cause:{}", Throwables.getStackTraceAsString(var11));
         result.setError("create.trade.pay.fail");
      }

      return result;
   }

   public Response createPayStage(PayStage payStage) {
      Response<Boolean> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(payStage), "pay.stage.object.is.null");
         this.payStageDao.create(payStage);
         result.setResult(Boolean.TRUE);
      } catch (IllegalArgumentException var4) {
         log.error("create pay stage fail error:{}", var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("create pay stage fail cause:{}", Throwables.getStackTraceAsString(var5));
         result.setError("create.pay.stage.fail");
      }

      return result;
   }

   public Response createPayChannel(PayChannel payChannel) {
      Response<PayChannel> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(payChannel), "pay.channel.object.is.null");
         PayChannel exit = this.payChannelDao.findByStageIdAndChannel(payChannel.getStageId(), payChannel.getChannel(), PayChannelType.PAID);
         if(Arguments.notNull(exit)) {
            PayChannel channel = new PayChannel();
            channel.setId(exit.getId());
            this.payChannelDao.update(channel);
            result.setResult(exit);
         } else {
            this.payChannelDao.create(payChannel);
            result.setResult(payChannel);
         }
      } catch (IllegalArgumentException var5) {
         log.error("create pay channel fail error:{}", var5.getMessage());
         result.setError(var5.getMessage());
      } catch (Exception var6) {
         log.error("create pay channel fail cause:{}", Throwables.getStackTraceAsString(var6));
         result.setError("create.pay.channel.fail");
      }

      return result;
   }

   public Response updatePayChannel(PayChannel payChannel) {
      Response<Boolean> result = new Response();

      try {
         this.payChannelDao.update(payChannel);
         result.setResult(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("update pay channel fail,cause:{}", Throwables.getStackTraceAsString(var4));
         result.setError("update.pay.channel.fail");
      }

      return result;
   }

   public Response createPayChannelForRefund(PayChannel payChannel) {
      Response<PayChannel> result = new Response();

      try {
         Preconditions.checkArgument(Arguments.notNull(payChannel), "pay.channel.object.is.null");
         PayChannel exist = this.payChannelDao.findRefundByRefundIdAndChannel(payChannel.getRefundOrderId(), payChannel.getChannel());
         if(Arguments.notNull(exist)) {
            PayChannel channel = new PayChannel();
            channel.setId(exist.getId());
            this.payChannelDao.update(channel);
            result.setResult(exist);
            log.warn("pay channel exist where refund id :{} channel :{}", payChannel.getRefundOrderId(), payChannel.getChannel());
         } else {
            this.payChannelDao.create(payChannel);
            result.setResult(payChannel);
         }
      } catch (IllegalArgumentException var5) {
         log.error("create pay channel fail error:{}", var5.getMessage());
         result.setError(var5.getMessage());
      } catch (Exception var6) {
         log.error("create pay channel fail cause:{}", Throwables.getStackTraceAsString(var6));
         result.setError("create.pay.channel.fail");
      }

      return result;
   }

   public Response updatePayStatus(PayChannel payChannel) {
      Response<Boolean> result = new Response();

      try {
         this.payManager.updatePayStatus(payChannel);
         result.setResult(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("update pay status fail paychannel: {} cause:{}", payChannel, Throwables.getStackTraceAsString(var4));
         result.setError("update.pay.status.fail");
      }

      return result;
   }

   public Response createOwnerPayChannel(OwnerPayChannel ownerPayChannel) {
      Response<Boolean> result = new Response();

      try {
         OwnerPayChannel opc = this.ownerPayChannelDao.findByOwnerIdAndType(ownerPayChannel.getOwnerId(), ownerPayChannel.getType());
         if(Arguments.notNull(opc)) {
            result.setResult(Boolean.TRUE);
            return result;
         }

         this.ownerPayChannelDao.create(ownerPayChannel);
         result.setResult(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("create owner pay channel fail, cause: {}", Throwables.getStackTraceAsString(var4));
         result.setError("create.owner.pay.channel.fail");
      }

      return result;
   }

   public Response updateOwnerPayChannel(OwnerPayChannel ownerPayChannel) {
      Response<Boolean> result = new Response();

      try {
         OwnerPayChannel opc = this.ownerPayChannelDao.findByOwnerIdAndType(ownerPayChannel.getOwnerId(), ownerPayChannel.getType());
         Preconditions.checkState(Arguments.notNull(opc), "owner.pay.channel.not.exist");
         this.ownerPayChannelDao.update(ownerPayChannel);
         result.setResult(Boolean.TRUE);
      } catch (IllegalStateException var4) {
         log.error("update owner pay channel fail, error: {}", var4.getMessage());
         result.setError(var4.getMessage());
      } catch (Exception var5) {
         log.error("update owner pay channel fail, cause: {}", Throwables.getStackTraceAsString(var5));
         result.setError("update.owner.pay.channel.fail");
      }

      return result;
   }

   public Response updatePayChannelStatusForRefund(String batchNO, PayChannelStatus from, PayChannelStatus to, Date date) {
      Response<Boolean> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(batchNO), "batch.no.invalid");
         Preconditions.checkArgument(Arguments.notNull(from), "pay.channel.status.invalid");
         Preconditions.checkArgument(Arguments.notNull(to), "pay.channel.status.invalid");
         Preconditions.checkArgument(Arguments.notNull(date), "pay.channel.refund.at.invalid");
         PayChannel exit = this.payChannelDao.findRefundByBatchNoAndStatus(batchNO, from);
         Preconditions.checkState(Arguments.notNull(exit), "pay.channel.not.exist");
         this.payChannelDao.setRefundSuccess(batchNO, from, to, date);
         result.setResult(Boolean.TRUE);
      } catch (IllegalArgumentException var7) {
         log.error("update pay channel status for refund where batch no: {} and status form: {} to status: {}，error: {}", new Object[]{batchNO, Integer.valueOf(from.value()), Integer.valueOf(to.value()), var7.getMessage()});
         result.setError(var7.getMessage());
      } catch (IllegalStateException var8) {
         log.error("update pay channel status for refund where batch no: {} and status form: {} to status: {}，error: {}", new Object[]{batchNO, Integer.valueOf(from.value()), Integer.valueOf(to.value()), var8.getMessage()});
         result.setError(var8.getMessage());
      } catch (Exception var9) {
         log.error("update pay channel status for refund where batch no: {} and status form: {} to status: {}，cause: {}", new Object[]{batchNO, Integer.valueOf(from.value()), Integer.valueOf(to.value()), Throwables.getStackTraceAsString(var9)});
         result.setError("update.pay.channel.fail");
      }

      return result;
   }

   public Response updateTradePayStatus(String systemNo, Integer status) {
      Response<Boolean> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(systemNo), "system.no.invalid");
         Preconditions.checkArgument(Arguments.notNull(status), "trade.pay.status.invalid");
         this.tradePayDao.updateStatus(systemNo, status);
         result.setResult(Boolean.TRUE);
      } catch (IllegalArgumentException var5) {
         log.error("udpate trade pay status where systemno: {} status: {} fail,error: {}", new Object[]{systemNo, status, var5.getMessage()});
      } catch (Exception var6) {
         log.error("udpate trade pay status where systemno: {} status: {} fail,cause: {}", new Object[]{systemNo, status, Throwables.getStackTraceAsString(var6)});
         result.setError("update.trade.pay.status.fail");
      }

      return result;
   }

   public Response createExceptionPayTracks(ExceptionPayTrack track) {
      Response<Boolean> result = new Response();

      try {
         this.exceptionPayTrackDao.create(track);
         result.setResult(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("create exception pay tracks: {} fail,cause:{}", track.toString(), Throwables.getStackTraceAsString(var4));
         result.setError("create.exception.pay.track.fail");
      }

      return result;
   }

   public Response createExceptionPayTracks(String tradeNo, ExceptionPayType type) {
      Response<Boolean> result = new Response();

      try {
         Preconditions.checkArgument(!Strings.isNullOrEmpty(tradeNo), "trade.no.is.null");
         PayChannel payChannel = this.payChannelDao.findByTradeNo(tradeNo, PayChannelType.PAID);
         Preconditions.checkState(Arguments.notNull(payChannel), "pay.channel.not.exist");
         ExceptionPayTrack track = new ExceptionPayTrack();
         track.setType(Integer.valueOf(type.value()));
         track.setChannel(payChannel.getChannel());
         track.setFee(payChannel.getFee());
         track.setPaymentCode(payChannel.getPaymentCode());
         track.setPaidAt(payChannel.getPaidAt());
         track.setStatus(Integer.valueOf(TrackHandleStatus.NOT.value()));
         track.setTradeNo(tradeNo);
         this.exceptionPayTrackDao.create(track);
         result.setResult(Boolean.TRUE);
      } catch (IllegalArgumentException var6) {
         log.error("create exception pay track where tradeNo:{} and type :{} fail,error:{}", new Object[]{tradeNo, type.toString(), var6.getMessage()});
         result.setError(var6.getMessage());
      } catch (IllegalStateException var7) {
         log.error("create exception pay track where tradeNo:{} and type :{} fail,error:{}", new Object[]{tradeNo, type.toString(), var7.getMessage()});
         result.setError(var7.getMessage());
      } catch (Exception var8) {
         log.error("create exception pay track where tradeNo:{} and type :{} fail,cause:{}", new Object[]{tradeNo, type.toString(), Throwables.getStackTraceAsString(var8)});
         result.setError("create.exception.pay.track.fail");
      }

      return result;
   }

   public Response updateExceptionPayTrackStatus(Long id, TrackHandleStatus status) {
      Response<Boolean> reslut = new Response();

      try {
         ExceptionPayTrack track = new ExceptionPayTrack();
         track.setId(id);
         track.setStatus(Integer.valueOf(status.value()));
         this.exceptionPayTrackDao.update(track);
         reslut.setResult(Boolean.TRUE);
      } catch (Exception var5) {
         log.error("update timeout pay track status:{} fail,cause:{}", Integer.valueOf(status.value()), Throwables.getStackTraceAsString(var5));
         reslut.setError("update.timeout.pay.track.fail");
      }

      return reslut;
   }

   public Response createAlipayTrans(AlipayTrans trans) {
      Response<Boolean> result = new Response();

      try {
         this.alipayTransDao.createNonexists(trans);
         result.setResult(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("create alipay trans fail,cause: {}", Throwables.getStackTraceAsString(var4));
         result.setError("create.alipay.trans.fail");
      }

      return result;
   }

   public Response createWechatPayTrans(WechatpayTrans trans) {
      Response<Boolean> result = new Response();

      try {
         this.wechatpayTransDao.createNotExists(trans);
         result.setResult(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("create wechat pay trans fail,cause: {}", Throwables.getStackTraceAsString(var4));
         result.setError("create.wechat.pay.trans.fail");
      }

      return result;
   }

   public Response createUnionpayTrans(UnionPayTrans trans) {
      Response<Boolean> result = new Response();

      try {
         if(Arguments.isNull(this.unionPayTransDao.findByQueryId(trans.getQueryId()))) {
            this.unionPayTransDao.create(trans);
         }

         result.setResult(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("create unionpay trans fail,cause: {}", Throwables.getStackTraceAsString(var4));
         result.setError("create.unionpay.trans.fail");
      }

      return result;
   }

   public Response createKjtpayTrans(KjtpayTrans trans) {
      Response<Boolean> result = new Response();

      try {
         if(Arguments.isNull(this.kjtpayTransDao.findByInnerNo(trans.getInnerNo()))) {
            this.kjtpayTransDao.create(trans);
         }

         result.setResult(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("create kjtpay trans fail,cause: {}", Throwables.getStackTraceAsString(var4));
         result.setError("create.kjtpay.trans.fail");
      }

      return result;
   }
}
