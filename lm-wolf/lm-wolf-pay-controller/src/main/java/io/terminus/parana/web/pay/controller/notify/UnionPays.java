package io.terminus.parana.web.pay.controller.notify;

import com.google.common.base.Preconditions;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;
import com.google.common.eventbus.EventBus;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.lib.pay.channel.unionpay.sdk.SDKUtil;
import io.terminus.parana.order.service.OrderReadService;
import io.terminus.parana.pay.enums.ExceptionPayType;
import io.terminus.parana.pay.model.PayChannel;
import io.terminus.parana.pay.model.TradePay;
import io.terminus.parana.pay.service.PayReadService;
import io.terminus.parana.pay.service.PayWriteService;
import io.terminus.parana.web.pay.event.PaidEvent;
import io.terminus.parana.web.pay.event.RefundedEvent;
import java.io.UnsupportedEncodingException;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import javax.servlet.http.HttpServletRequest;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping({"/api/unionpay"})
public class UnionPays {
   private static final Logger log = LoggerFactory.getLogger(UnionPays.class);
   @Autowired
   private OrderReadService orderReadService;
   @Autowired
   private PayWriteService payWriteService;
   @Autowired
   private PayReadService payReadService;
   @Value("${pay.debug:false}")
   private String payDebug;
   @Autowired
   private EventBus eventBus;
   private static final DateTimeFormatter DFT = DateTimeFormat.forPattern("yyyyMMddHHmmss");

   @RequestMapping(
      value = {"/notify"},
      produces = {"text/plain"}
   )
   @ResponseBody
   public String payNotify(HttpServletRequest request) {
      try {
         log.info("unionpay-接收后台通知开始");
         request.setCharacterEncoding("ISO-8859-1");
         this.checkPayNotifyArguments(request);
         Map<String, String> valideData = this.getValideData(request);
         if(!this.payDebug.equals("true")) {
            this.validateRequestSign(request, valideData);
         }

         this.checkTradeStatusIfSucceed(request);
         String orderId = (String)valideData.get("orderId");
         log.info("unionpay-接收后台通知结束");
         log.info("更新订单信息开始 trade no:({})", orderId);
         this.paid(valideData);
         log.info("更新订单信息结束 trade no:({})", orderId);
         return "ok";
      } catch (IllegalArgumentException var4) {
         log.warn("UnionPay notify raise error params:{}, error:{} ", request.getParameterMap(), var4.getMessage());
      } catch (IllegalStateException var5) {
         log.warn("UnionPay notify raise error params:{}, error:{} ", request.getParameterMap(), var5.getMessage());
      } catch (Exception var6) {
         log.error("UnionPay notify raise error params:{}, cause:{}", request.getParameterMap(), Throwables.getStackTraceAsString(var6));
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
         Map<String, String> valideData = this.getValideData(request);
         if(!this.payDebug.equals("true")) {
            this.validateRequestSign(request, valideData);
         }

         this.updateRefundSucceed(valideData);
         return "ok";
      } catch (IllegalArgumentException var3) {
         log.error("unionpay Refund notify raise error params:{}, error:{}", request.getParameterMap(), var3.getMessage());
      } catch (IllegalStateException var4) {
         log.error("unionpay Refund notify raise error params:{}, error:{}", request.getParameterMap(), var4.getMessage());
      } catch (Exception var5) {
         log.error("unionpay Refund notify raise error params:{}, cause:{}", request.getParameterMap(), Throwables.getStackTraceAsString(var5));
      }

      return "fail";
   }

   public static Map getAllRequestParam(HttpServletRequest request) {
      Map<String, String> res = new HashMap();
      Enumeration<?> temp = request.getParameterNames();
      if(null != temp) {
         while(temp.hasMoreElements()) {
            String en = (String)temp.nextElement();
            String value = request.getParameter(en);
            res.put(en, value);
            if(null == res.get(en) || "".equals(res.get(en))) {
               res.remove(en);
            }
         }
      }

      return res;
   }

   public Map getValideData(HttpServletRequest request) throws UnsupportedEncodingException {
      String encoding = request.getParameter("encoding");
      Map<String, String> reqParam = getAllRequestParam(request);
      Map<String, String> valideData = null;
      if(null != reqParam && !reqParam.isEmpty()) {
         Iterator<Entry<String, String>> it = reqParam.entrySet().iterator();
         valideData = new HashMap(reqParam.size());

         while(it.hasNext()) {
            Entry<String, String> e = (Entry)it.next();
            String key = (String)e.getKey();
            String value = (String)e.getValue();
            value = new String(value.getBytes("ISO-8859-1"), encoding);
            valideData.put(key, value);
         }
      }

      return valideData;
   }

   private void checkPayNotifyArguments(HttpServletRequest request) {
      log.debug("pay request param map: {}", request.getParameterMap());
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("respCode")), "resp.code.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("orderId")), "order.id.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("queryId")), "query.id.empty");
   }

   private void validateRequestSign(HttpServletRequest request, Map valideData) {
      String encoding = request.getParameter("encoding");
      Preconditions.checkState(SDKUtil.validate(valideData, encoding), "unionpay-验证签名结果[失败].");
   }

   private void checkTradeStatusIfSucceed(HttpServletRequest request) {
      String tradeStatus = request.getParameter("respCode");
      Preconditions.checkState(this.isTradeSucceed(tradeStatus), "resp.code.incorrect");
   }

   private boolean isTradeSucceed(String tradeStatus) {
      return Arguments.equalWith(tradeStatus, "00");
   }

   private Date getPaidAt(String paidTime) {
      try {
         return DateTime.parse(paidTime, DFT).toDate();
      } catch (Exception var3) {
         log.error("fail to get paidAt, cause:{}", Throwables.getStackTraceAsString(var3));
         return null;
      }
   }

   private void paid(Map valideData) {
      String tradeNo = (String)valideData.get("orderId");
      Preconditions.checkArgument(Arguments.notEmpty(tradeNo), "trade.no.invalid");
      TradePay tradePay = this.getTradePay(tradeNo);
      PayChannel payChannel = this.getPayChannel(tradeNo);
      List<Long> orderIds = tradePay.getPaidOrderIds();
      Preconditions.checkState(Arguments.notEmpty(orderIds), "order.id.invalid");
      if(payChannel.getStatus().equals(Integer.valueOf(1))) {
         log.info("payChannel(id:{}, tradeNo:{}, status:{})  paid, skipped it", new Object[]{payChannel.getId(), tradeNo, payChannel.getStatus()});
      } else {
         String payCode = (String)valideData.get("queryId");
         Date paidAt = this.getPaidAt((String)valideData.get("txnTime"));
         if(Arguments.isNull(paidAt)) {
            throw new ServiceException("txn.time.invalid");
         } else {
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
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("respCode")), "resp.code.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("orderId")), "order.id.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("queryId")), "query.id.empty");
      Preconditions.checkArgument(Arguments.notEmpty(request.getParameter("encoding")), "encoding.empty");
   }

   private void updateRefundSucceed(Map valideData) {
      String batchNo = (String)valideData.get("orderId");
      Response<PayChannel> payChannelR = this.payReadService.findPayChannelByBatchNoForRefund(batchNo);
      if(!payChannelR.isSuccess()) {
         log.warn("fail to find pay channel by batch no {} when refund success notify, error code:{}", batchNo, payChannelR.getError());
         throw new JsonResponseException(payChannelR.getError());
      } else {
         PayChannel payChannel = (PayChannel)payChannelR.getResult();
         if(payChannel.getStatus().equals(Integer.valueOf(1))) {
            log.info("payChannel(id:{}, payment code:{}, status:{})  refund, skipped it", new Object[]{payChannel.getId(), payChannel.getPaymentCode(), payChannel.getStatus()});
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
