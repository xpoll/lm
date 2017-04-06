package cn.blmdz.wolf.web.pay.controller.notify;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import com.google.common.base.Preconditions;
import com.google.common.base.Splitter;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;
import com.google.common.eventbus.EventBus;

import cn.blmdz.aide.pay.channel.alipay.dto.settlement.AlipayWapResponse;
import cn.blmdz.aide.pay.channel.alipay.request.AlipayToken;
import cn.blmdz.aide.pay.channel.alipay.request.MobileTokenRequest;
import cn.blmdz.aide.pay.channel.alipay.request.Request;
import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.pay.enums.ExceptionPayType;
import cn.blmdz.wolf.pay.model.PayChannel;
import cn.blmdz.wolf.pay.model.TradePay;
import cn.blmdz.wolf.pay.service.PayReadService;
import cn.blmdz.wolf.pay.service.PayWriteService;
import cn.blmdz.wolf.web.pay.event.PaidEvent;
import cn.blmdz.wolf.web.pay.event.RefundedEvent;
import cn.blmdz.wolf.web.pay.service.PaySettingsProvider;

@Controller
@RequestMapping({"/api/alipay"})
public class Alipays {
   private static final Logger log = LoggerFactory.getLogger(Alipays.class);
   private final PayReadService payReadService;
   private final PayWriteService payWriteService;
   private final OrderReadService orderReadService;
   private final PaySettingsProvider paySettingsProvider;
   @Autowired
   private EventBus eventBus;
   private static final Splitter tradeSplitter = Splitter.on("#");
   private static final Splitter infoSplitter = Splitter.on("^");
   private static final DateTimeFormatter DFT = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");
   private static final String ALIPAY_WAP_OLD_OVERSION = "alipay.wap.trade.create.direct";

   @Autowired
   public Alipays(PayReadService payReadService, PayWriteService payWriteService, OrderReadService orderReadService, PaySettingsProvider paySettingsProvider) {
      this.payReadService = payReadService;
      this.payWriteService = payWriteService;
      this.orderReadService = orderReadService;
      this.paySettingsProvider = paySettingsProvider;
   }

   @RequestMapping(
      value = {"/notify"},
      produces = {"text/plain"}
   )
   @ResponseBody
   public String payNotify(HttpServletRequest request) {
      try {
         this.checkPayNotifyArguments(request);
         this.validateRequestSign(request);
         this.checkTradeStatusIfSucceed(request);
         this.paid(request);
         return "success";
      } catch (IllegalArgumentException var3) {
         log.warn("Pay notify raise error params:{}, error:{} ", request.getParameterMap(), var3.getMessage());
      } catch (IllegalStateException var4) {
         log.warn("Pay notify raise error params:{}, error:{} ", request.getParameterMap(), var4.getMessage());
      } catch (Exception var5) {
         log.error("Pay notify raise error params:{}, cause:{}", request.getParameterMap(), Throwables.getStackTraceAsString(var5));
      }

      return "fail";
   }

   @RequestMapping(
      value = {"/refund/notify"},
      produces = {"text/plain"}
   )
   @ResponseBody
   public String refundNotify(HttpServletRequest request) {
      try {
         log.info("refund notify param map:{}", request.getParameterMap());
         this.checkRefundNotifyArguments(request);
         this.validateRequestSign(request);
         this.updateRefundSucceed(request);
         return "success";
      } catch (IllegalArgumentException var3) {
         log.warn("Refund notify raise error params:{}, error:{}", request.getParameterMap(), var3.getMessage());
      } catch (IllegalStateException var4) {
         log.warn("Refund notify raise error params:{}, error:{}", request.getParameterMap(), var4.getMessage());
      } catch (Exception var5) {
         log.error("Refund notify raise error params:{}, cause:{}", request.getParameterMap(), Throwables.getStackTraceAsString(var5));
      }

      return "fail";
   }

   private void checkPayNotifyArguments(HttpServletRequest request) {
      log.debug("pay request param map: {}", request.getParameterMap());
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("trade_status")), "trade.status.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("out_trade_no")), "out.trade.no.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("trade_no")), "trade.no.empty");
   }

   private void validateRequestSign(HttpServletRequest request) {
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("sign")), "sign.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("sign_type")), "sign.type.empty");
      String sign = request.getParameter("sign");
      Map<String, String> params = Maps.newTreeMap();

      for(String key : request.getParameterMap().keySet()) {
         String value = request.getParameter(key);
         if(!this.isValueEmptyOrSignRelatedKey(key, value)) {
            params.put(key, value);
         }
      }

      AlipayToken token = this.paySettingsProvider.buildAlipayToken();
      boolean valid = Request.verify(params, sign, token);
      Preconditions.checkState(valid, "");
   }

   private boolean isValueEmptyOrSignRelatedKey(String key, String value) {
      return Arguments.isEmpty(value) || StringUtils.equalsIgnoreCase(key, "sign") || StringUtils.equalsIgnoreCase(key, "sign_type");
   }

   private void checkTradeStatusIfSucceed(HttpServletRequest request) {
      String tradeStatus = request.getParameter("trade_status");
      Preconditions.checkState(this.isTradeSucceed(tradeStatus), "trade.status.incorrect");
   }

   private boolean isTradeSucceed(String tradeStatus) {
      return Arguments.equalWith(tradeStatus, "TRADE_SUCCESS") || Arguments.equalWith(tradeStatus, "TRADE_FINISHED");
   }

   private Date getPaidAt(HttpServletRequest request) {
      try {
         String paidTime = request.getParameter("gmt_payment");
         return DFT.parseDateTime(paidTime).toDate();
      } catch (Exception var3) {
         log.error("fail to get paidAt, cause:{}", Throwables.getStackTraceAsString(var3));
         return null;
      }
   }

   private Date getPaidAt(AlipayWapResponse response) {
      try {
         String paidTime = response.getGmtPayment();
         return DFT.parseDateTime(paidTime).toDate();
      } catch (Exception var3) {
         log.error("fail to get paidAt, cause:{}", Throwables.getStackTraceAsString(var3));
         return DateTime.now().toDate();
      }
   }

   private void paid(HttpServletRequest request) {
      AlipayWapResponse response = null;
      if(Objects.equals("alipay.wap.trade.create.direct", request.getParameter("service"))) {
         String notifyData = request.getParameter("notify_data");
         response = (AlipayWapResponse)MobileTokenRequest.parse(notifyData, AlipayWapResponse.class);
      }

      String tradeNo = Arguments.isNull(response)?request.getParameter("out_trade_no"):response.getOutTradeNo();
      Preconditions.checkArgument(Arguments.notEmpty(tradeNo), "trade.no.invalid");
      TradePay tradePay = this.getTradePay(tradeNo);
      PayChannel payChannel = this.getPayChannel(tradeNo);
      List<Long> orderIds = tradePay.getPaidOrderIds();
      Preconditions.checkState(Arguments.notEmpty(orderIds), "order.id.invalid");
      if(payChannel.getStatus().equals(Integer.valueOf(1))) {
         log.info("payChannel(id:{}, tradeNo:{}, status:{})  paid, skipped it", new Object[]{payChannel.getId(), tradeNo, payChannel.getStatus()});
      } else {
         String payCode = request.getParameter("trade_no");
         Date paidAt = Arguments.isNull(response)?this.getPaidAt(request):this.getPaidAt(response);
         payChannel.setPaymentCode(payCode);
         payChannel.setPaidAt(paidAt);
         payChannel.setStatus(Integer.valueOf(1));
         Response<Boolean> updateStatusRes = this.payWriteService.updatePayStatus(payChannel);
         if(!updateStatusRes.isSuccess()) {
            throw new ServiceException(updateStatusRes.getError());
         } else {
            Response<Boolean> isMultiRes = this.payReadService.isMultiplePay(payChannel.getTradeNo());
            if(!isMultiRes.isSuccess()) {
               log.error("valid is multiple pay where trade no: {} fail,error: {}", payChannel.getTradeNo(), isMultiRes.getError());
               throw new ServiceException(isMultiRes.getError());
            } else {
               Boolean isMulti = (Boolean)isMultiRes.getResult();
               if(isMulti.booleanValue()) {
                  Response<Boolean> exceptionPayRes = this.payWriteService.createExceptionPayTracks(payChannel.getTradeNo(), ExceptionPayType.MULTIPLE_PAY);
                  if(!exceptionPayRes.isSuccess()) {
                     log.error("create exception pay tracks where trade no: {} ,exception type: {},fail,error:{}", new Object[]{payChannel.getTradeNo(), Integer.valueOf(ExceptionPayType.MULTIPLE_PAY.value()), exceptionPayRes.getError()});
                     throw new ServiceException(exceptionPayRes.getError());
                  }
               }

               Map<String, Object> updateParams = Maps.newHashMap();
               updateParams.put("channel", payChannel.getChannel());
               updateParams.put("tradeNo", payChannel.getTradeNo());
               updateParams.put("systemNo", tradePay.getSystemNo());
               updateParams.put("paymentCode", payChannel.getPaymentCode());
               this.eventBus.post(new PaidEvent(updateParams, payChannel, orderIds, tradePay));
            }
         }
      }
   }

   private TradePay getTradePay(String tradeNo) {
      Response<TradePay> res = this.payReadService.findTradePayByTradeNo(tradeNo);
      Preconditions.checkState(res.isSuccess(), res.getError());
      return (TradePay)res.getResult();
   }

   private PayChannel getPayChannel(String tradeNo) {
      Response<PayChannel> res = this.payReadService.findPayChannelByTradeNoForPaid(tradeNo);
      Preconditions.checkState(res.isSuccess(), res.getError());
      return (PayChannel)res.getResult();
   }

   private void checkRefundNotifyArguments(HttpServletRequest request) {
      log.debug("pay request param map: {}", request.getParameterMap());
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("batch_no")), "alipay.refund.batch.no.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("result_details")), "alipay.refund.result.detail.empty");
   }

   private void updateRefundSucceed(HttpServletRequest request) {
      String refundDetail = request.getParameter("result_details");
      List<String> details = tradeSplitter.splitToList(refundDetail);
      Preconditions.checkState(details.size() == 1 || details.size() == 2, "alipay.refund.detail.num.incorrect");
      String detail = (String)details.get(0);
      List<String> fields = infoSplitter.splitToList(detail);
      Preconditions.checkState(fields.size() >= 3, "alipay.refund.detail.field.num.incorrect");
      String result = (String)fields.get(2);
      Preconditions.checkState(StringUtils.equalsIgnoreCase(result, "SUCCESS"), "alipay.refund.fail");
      String batchNo = request.getParameter("batch_no");
      Response<PayChannel> payChannelR = this.payReadService.findPayChannelByBatchNoForRefund(batchNo);
      if(!payChannelR.isSuccess()) {
         log.warn("fail to find pay channel by batch no {} when refund success notify, error code:{}", batchNo, payChannelR.getError());
         throw new ServiceException(payChannelR.getError());
      } else {
         PayChannel payChannel = (PayChannel)payChannelR.getResult();
         if(payChannel.getStatus().equals(Integer.valueOf(1))) {
            log.info("payChannel(id:{}, batchNo:{}, status:{})  paid, skipped it", new Object[]{payChannel.getId(), batchNo, payChannel.getStatus()});
         } else {
            PayChannel updatePayChannel = new PayChannel();
            updatePayChannel.setId(payChannel.getId());
            updatePayChannel.setStatus(Integer.valueOf(1));
            updatePayChannel.setRefundAt(new Date());
            Response<Boolean> updateStatusRes = this.payWriteService.updatePayChannel(updatePayChannel);
            if(!updateStatusRes.isSuccess()) {
               throw new ServiceException(updateStatusRes.getError());
            } else {
               Map<String, Object> updateParams = Maps.newHashMap();
               updateParams.put("orderId", payChannel.getRefundOrderId());
               updateParams.put("batchNo", batchNo);
               updateParams.put("channel", payChannel.getChannel());
               updateParams.put("tradeNo", payChannel.getTradeNo());
               this.eventBus.post(new RefundedEvent(updateParams, payChannel));
            }
         }
      }
   }
}
