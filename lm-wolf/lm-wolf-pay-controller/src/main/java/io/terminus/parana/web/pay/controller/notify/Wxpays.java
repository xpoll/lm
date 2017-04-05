package io.terminus.parana.web.pay.controller.notify;

import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;
import com.google.common.eventbus.EventBus;
import com.google.common.io.BaseEncoding;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.common.utils.MapBuilder;
import io.terminus.lib.pay.channel.wechatpay.dto.WxPayNotifyDto;
import io.terminus.lib.pay.channel.wechatpay.request.WxPrePayRequest;
import io.terminus.lib.pay.channel.wechatpay.request.WxRequest;
import io.terminus.lib.pay.channel.wechatpay.request.WxToken;
import io.terminus.parana.pay.enums.ExceptionPayType;
import io.terminus.parana.pay.model.PayChannel;
import io.terminus.parana.pay.model.TradePay;
import io.terminus.parana.pay.service.PayReadService;
import io.terminus.parana.pay.service.PayWriteService;
import io.terminus.parana.web.pay.event.PaidEvent;
import io.terminus.parana.web.pay.service.PaySettingsProvider;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping({"/api/wxpay"})
public class Wxpays {
   private static final Logger log = LoggerFactory.getLogger(Wxpays.class);
   private final PayReadService payReadService;
   private final PayWriteService payWriteService;
   @Value("${pay.debug:false}")
   private String payDebug;
   private final PaySettingsProvider paySettingsProvider;
   private final EventBus eventBus;
   private static final Joiner BLANK = Joiner.on("").skipNulls();
   private static MessageDigest messageDigest = null;

   @Autowired
   public Wxpays(PayReadService payReadService, PayWriteService payWriteService, PaySettingsProvider paySettingsProvider, EventBus eventBus) {
      this.payReadService = payReadService;
      this.payWriteService = payWriteService;
      this.paySettingsProvider = paySettingsProvider;
      this.eventBus = eventBus;
   }

   @RequestMapping(
      value = {"/checkSignature"},
      method = {RequestMethod.GET}
   )
   @ResponseBody
   public String wxCheckSignature(@RequestParam("signature") String signature, @RequestParam("timestamp") String timestamp, @RequestParam("nonce") String nonce, @RequestParam("echostr") String echostr) {
      String token = this.paySettingsProvider.getWechatSettings().getToken();
      String[] paramArray = new String[]{token, timestamp, nonce};
      Arrays.sort(paramArray);
      String content = BLANK.join(paramArray);
      if(messageDigest != null) {
         byte[] encryptedArray = messageDigest.digest(content.getBytes());
         String hexadecimalString = BaseEncoding.base16().encode(encryptedArray);
         if(hexadecimalString != null && signature.equalsIgnoreCase(hexadecimalString)) {
            return echostr;
         }
      }

      return null;
   }

   @RequestMapping({"/notify"})
   @ResponseBody
   public String payNotify(@RequestBody String requestBody, HttpServletRequest request) {
      try {
         this.checkPayNotifyArguments(requestBody);
         if(!this.payDebug.equals("true")) {
            this.validateRequestSign(requestBody);
         }

         this.checkTradeStatusIfSucceed(requestBody);
         this.paid(requestBody, request);
         return this.result();
      } catch (IllegalArgumentException var4) {
         log.error("wx Pay notify raise error requestBody:{}, error:{} ", requestBody, var4.getMessage());
      } catch (IllegalStateException var5) {
         log.error("wx Pay notify raise error requestBody:{}, error:{} ", requestBody, var5.getMessage());
      } catch (Exception var6) {
         log.error("wx Pay notify raise error requestBody:{}, cause:{}", requestBody, Throwables.getStackTraceAsString(var6));
      }

      return "fail";
   }

   private void checkPayNotifyArguments(String requestBody) {
      log.debug("wxpay request body : {}", requestBody);
      Preconditions.checkArgument(Arguments.notEmpty(requestBody), "trade.requestBody.empty");
   }

   private void validateRequestSign(String requestBody) {
      WxToken token = this.paySettingsProvider.buildWxToken();
      boolean valid = WxRequest.verify(requestBody, token);
      Preconditions.checkState(valid, "notify.sign.check.fail");
   }

   private void checkTradeStatusIfSucceed(String requestBody) {
      WxPayNotifyDto wxPayNotifyDto = (WxPayNotifyDto)WxRequest.parse(requestBody, WxPayNotifyDto.class);
      Preconditions.checkState(wxPayNotifyDto.isSuccess(), "trade.status.incorrect");
   }

   private void paid(String requestBody, HttpServletRequest request) {
      WxToken token = this.paySettingsProvider.buildWxToken();
      if(!this.payDebug.equals("true")) {
         boolean verify = WxPrePayRequest.verify(requestBody, token);
         Preconditions.checkState(verify, "Illegal wx pay result:%s", new Object[]{requestBody});
      }

      WxPayNotifyDto wxPayNotifyDto = (WxPayNotifyDto)WxRequest.parse(requestBody, WxPayNotifyDto.class);
      TradePay tradePay = this.getTradePay(wxPayNotifyDto.getOutTradeNo());
      PayChannel payChannel = this.getPayChannel(wxPayNotifyDto.getOutTradeNo());
      List<Long> orderIds = tradePay.getPaidOrderIds();
      Preconditions.checkState(Arguments.notEmpty(orderIds), "order.id.invalid");
      if(!Arguments.notNull(payChannel) || !payChannel.getStatus().equals(Integer.valueOf(1))) {
         String payCode = wxPayNotifyDto.getTransactionId();
         payChannel.setPaymentCode(payCode);
         payChannel.setPaidAt(DateTime.now().toDate());
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

   private String result() {
      Map map = MapBuilder.newTreeMap().put("return_code", "SUCCESS").put("return_msg", "").map();
      return WxRequest.toXml(map);
   }

   @RequestMapping({"/warning"})
   @ResponseBody
   public String warning(@RequestBody String requestBody, HttpServletRequest request) {
      log.warn("wx warning requestBody: {}, requestMap: {}", requestBody, request.getParameterMap());
      return "";
   }

   static {
      try {
         messageDigest = MessageDigest.getInstance("SHA-1");
      } catch (NoSuchAlgorithmException var1) {
         log.error("Get SHA-1 encryption fail,cause {}", Throwables.getStackTraceAsString(var1));
      }

   }
}
