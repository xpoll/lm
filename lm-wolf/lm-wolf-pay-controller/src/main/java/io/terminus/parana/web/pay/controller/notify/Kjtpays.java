package io.terminus.parana.web.pay.controller.notify;

import com.google.common.base.Preconditions;
import com.google.common.base.Stopwatch;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;
import com.google.common.eventbus.EventBus;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.lib.pay.channel.kjtpay.pojo.VerifyResult;
import io.terminus.lib.pay.channel.kjtpay.verify.verifyClient;
import io.terminus.parana.order.service.OrderReadService;
import io.terminus.parana.pay.enums.ExceptionPayType;
import io.terminus.parana.pay.model.PayChannel;
import io.terminus.parana.pay.model.TradePay;
import io.terminus.parana.pay.service.PayReadService;
import io.terminus.parana.pay.service.PayWriteService;
import io.terminus.parana.web.pay.event.PaidEvent;
import io.terminus.parana.web.pay.event.RefundedEvent;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import javax.servlet.http.HttpServletRequest;
import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping({"/api/kjtpay"})
public class Kjtpays {
   private static final Logger log = LoggerFactory.getLogger(Kjtpays.class);
   @Autowired
   private PayWriteService payWriteService;
   @Autowired
   private PayReadService payReadService;
   @Value("${pay.debug:false}")
   private String payDebug;
   @Autowired
   private OrderReadService orderReadService;
   @Autowired
   private EventBus eventBus;
   private DateTimeFormatter NUM_FMT = DateTimeFormat.forPattern("YYYYMMddHHmmss");
   private DateTimeFormatter DTF = DateTimeFormat.forPattern("yyyy-MM-dd");

   @RequestMapping(
      value = {"/notify"},
      produces = {"text/plain"}
   )
   @ResponseBody
   public String payNotify(HttpServletRequest request) {
      log.info("KJTPAY-NOTIFY invoking 。。。。");
      log.debug("pay request param map: {}", request.getParameterMap());

      try {
         this.checkPayNotifyArguments(request);
         if(!this.payDebug.equals("true")) {
            this.validateRequestSign(request);
         }

         this.checkTradeStatusIfSucceed(request);
         this.paid(request);
         return "success";
      } catch (IllegalArgumentException var3) {
         log.error("Pay notify raise error params:{}, error:{} ", request.getParameterMap(), var3.getMessage());
      } catch (IllegalStateException var4) {
         log.error("Pay notify raise error params:{}, error:{} ", request.getParameterMap(), var4.getMessage());
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
      log.info("KJTPAY-REFUND-NOTIFY invoking 。。。。");

      try {
         log.info("kjtpay refund notify param map:{}", request.getParameterMap());
         this.checkRefundNotifyArguments(request);
         if(!this.payDebug.equals("true")) {
            this.validateRequestSign(request);
         }

         this.updateRefundSucceed(request);
         return "success";
      } catch (IllegalArgumentException var3) {
         log.error("Refund notify raise error params:{}, error:{}", request.getParameterMap(), var3.getMessage());
      } catch (IllegalStateException var4) {
         log.error("Refund notify raise error params:{}, error:{}", request.getParameterMap(), var4.getMessage());
      } catch (Exception var5) {
         log.error("Refund notify raise error params:{}, cause:{}", request.getParameterMap(), Throwables.getStackTraceAsString(var5));
      }

      return "fail";
   }

   private void checkPayNotifyArguments(HttpServletRequest request) {
      log.debug("pay request param map: {}", request.getParameterMap());
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("trade_status")), "trade.status.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("outer_trade_no")), "outer.trade.no.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("inner_trade_no")), "inner.trade.no.no.empty");
   }

   private void validateRequestSign(HttpServletRequest request) throws Exception {
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("sign")), "sign.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("sign_type")), "sign.type.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("_input_charset")), "input.charset.empty");
      String inputCharset = request.getParameter("_input_charset");
      Map<String, String> params = Maps.newTreeMap();

      for(String key : request.getParameterMap().keySet()) {
         String value = request.getParameter(key);
         if(!this.isValueEmptyOrSignRelatedKey(key)) {
            params.put(key, value);
         }
      }

      VerifyResult result = verifyClient.verifyBasic(inputCharset, params);
      if(result.isSuccess()) {
         log.debug("签名验证成功！");
      }

      Preconditions.checkState(result.isSuccess(), "");
   }

   private boolean isValueEmptyOrSignRelatedKey(String key) {
      return Arguments.isEmpty(key);
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
         String paidTime = request.getParameter("gmt_create");
         return DateTime.parse(paidTime, this.NUM_FMT).toDate();
      } catch (Exception var3) {
         log.error("fail to get paidAt, cause:{}", Throwables.getStackTraceAsString(var3));
         return DateTime.now().toDate();
      }
   }

   private void paid(HttpServletRequest request) {
      String tradeNo = request.getParameter("outer_trade_no");
      Preconditions.checkArgument(Arguments.notEmpty(tradeNo), "trade.no.invalid");
      TradePay tradePay = this.getTradePay(tradeNo);
      PayChannel payChannel = this.getPayChannel(tradeNo);
      List<Long> orderIds = tradePay.getPaidOrderIds();
      Preconditions.checkState(Arguments.notEmpty(orderIds), "order.id.invalid");
      if(payChannel.getStatus().equals(Integer.valueOf(1))) {
         log.info("payChannel(id:{}, tradeNo:{}, status:{})  paid, skipped it", new Object[]{payChannel.getId(), tradeNo, payChannel.getStatus()});
      } else {
         String payCode = request.getParameter("inner_trade_no");
         Date paidAt = this.getPaidAt(request);
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
               updateParams.put("paymentCode", payChannel.getPaymentCode());
               updateParams.put("systemNo", tradePay.getSystemNo());
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
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("orig_outer_trade_no")), "kjtpay.refund.orig.outer.trade.no.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("outer_trade_no")), "kjtpay.refund.outer.trade.no.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("inner_trade_no")), "kjtpay.refund.inner.trade.no.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("refund_status")), "kjtpay.refund.refund.status.empty");
   }

   private void updateRefundSucceed(HttpServletRequest request) {
      String batchNo = request.getParameter("outer_trade_no");
      String result = request.getParameter("refund_status");
      Preconditions.checkState(StringUtils.equalsIgnoreCase(result, "REFUND_SUCCESS"), "kjtpay.refund.fail");
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

   @RequestMapping(
      value = {"/syncKjtpayTrans"},
      method = {RequestMethod.GET}
   )
   @ResponseBody
   public void syncKjtpayTrans(@RequestParam(
   required = false
) String date) {
      if(Strings.isNullOrEmpty(date)) {
         Date dateTime = DateTime.now().minusDays(1).toDate();
      } else {
         Date dateTime = this.DTF.parseDateTime(date).toDate();
      }

      Integer pageNo = Integer.valueOf(1);

      try {
         log.info("[CRON-JOB] loadByDay kjtpay job begin");
         Stopwatch watch = Stopwatch.createStarted();

         while(true) {
            Response<Boolean> loadedResp = null;
            if(!loadedResp.isSuccess()) {
               break;
            }

            pageNo = Integer.valueOf(pageNo.intValue() + 1);
            if(!((Boolean)loadedResp.getResult()).booleanValue()) {
               break;
            }
         }

         watch.stop();
         log.info("[CRON-JOB] loadByDay kjtpay job end, cost time: {} ms", Long.valueOf(watch.elapsed(TimeUnit.MILLISECONDS)));
      } catch (Exception var8) {
         log.error("Error raise when loadLastDay", var8);
      }

   }
}
