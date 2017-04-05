package io.terminus.parana.web.pay.service;

import com.fasterxml.jackson.databind.JavaType;
import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.eventbus.EventBus;
import com.thoughtworks.xstream.XStream;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.common.utils.Joiners;
import io.terminus.common.utils.JsonMapper;
import io.terminus.lib.file.ImageServer;
import io.terminus.lib.pay.channel.alipay.dto.RedirectInfo;
import io.terminus.lib.pay.channel.alipay.enums.RefundInterfaceType;
import io.terminus.lib.pay.channel.alipay.manager.AlipayManager;
import io.terminus.lib.pay.channel.alipay.request.AlipaySyncResponse;
import io.terminus.lib.pay.channel.alipay.request.AlipayToken;
import io.terminus.lib.pay.channel.alipay.request.RefundRequest;
import io.terminus.lib.pay.channel.kjtpay.manager.KjtpayManager;
import io.terminus.lib.pay.channel.kjtpay.request.KjtToken;
import io.terminus.lib.pay.channel.unionpay.manager.UnionpayManager;
import io.terminus.lib.pay.channel.unionpay.request.UnionToken;
import io.terminus.lib.pay.channel.unionpay.sdk.SDKConfig;
import io.terminus.lib.pay.channel.unionpay.sdk.SDKUtil;
import io.terminus.lib.pay.channel.wechatpay.HttpsUtil;
import io.terminus.lib.pay.channel.wechatpay.dto.WxPayRefundResponse;
import io.terminus.lib.pay.channel.wechatpay.dto.WxPrePayResponse;
import io.terminus.lib.pay.channel.wechatpay.manager.WechatpayManager;
import io.terminus.lib.pay.channel.wechatpay.request.WxRequest;
import io.terminus.lib.pay.channel.wechatpay.request.WxToken;
import io.terminus.lib.pay.utils.GenerateRandom;
import io.terminus.pampas.engine.ThreadVars;
import io.terminus.parana.file.util.FileUtil;
import io.terminus.parana.pay.dto.PayStageInfo;
import io.terminus.parana.pay.enums.PayChannelBusinessType;
import io.terminus.parana.pay.enums.PayChannelStatus;
import io.terminus.parana.pay.enums.PayChannelType;
import io.terminus.parana.pay.model.PayChannel;
import io.terminus.parana.pay.model.PayStage;
import io.terminus.parana.pay.model.TradePay;
import io.terminus.parana.pay.service.PayReadService;
import io.terminus.parana.pay.service.PayWriteService;
import io.terminus.parana.web.pay.dto.AlipaySettingsDto;
import io.terminus.parana.web.pay.dto.KjtSettingsDto;
import io.terminus.parana.web.pay.dto.UnionSettingsDto;
import io.terminus.parana.web.pay.dto.WechatSettingsDto;
import io.terminus.parana.web.pay.event.RefundedEvent;
import io.terminus.parana.web.pay.service.PaySettingsProvider;
import io.terminus.parana.web.pay.service.PayWebService;
import io.terminus.parana.web.pay.util.QrHelper;
import java.io.File;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class DefaultPayWebService implements PayWebService {
   private static final Logger log = LoggerFactory.getLogger(DefaultPayWebService.class);
   @Autowired
   private PayWriteService payWriteService;
   @Autowired
   private PayReadService payReadService;
   @Autowired
   private ImageServer imageServer;
   @Autowired
   private PaySettingsProvider paySettingsProvider;
   @Value("${pay.debug:false}")
   private String payDebug;
   @Autowired
   private EventBus eventBus;
   private static final DateTimeFormatter DFT = DateTimeFormat.forPattern("yyyyMMddHHmmss");
   private static XStream xStream = new XStream();
   private static final JsonMapper JSON_MAPPER = JsonMapper.nonDefaultMapper();
   private static final JavaType INSTALMENT_DATA = JSON_MAPPER.createCollectionType(Map.class, new Class[]{String.class, String.class});

   public Response refundRequest(Long refundTrackId, Integer orderType, String tradeNo, Integer refundAmount, String reason, PayChannelBusinessType type) {
      Response<Boolean> result = new Response();

      try {
         Response<PayChannel> oldPayChannelRes = this.payReadService.findPayChannelByTradeNoForPaid(tradeNo);
         if(!oldPayChannelRes.isSuccess()) {
            throw new ServiceException(oldPayChannelRes.getError());
         }

         PayChannel oldPayChannel = (PayChannel)oldPayChannelRes.getResult();
         String channel = oldPayChannel.getChannel();
         PayChannel payChannel = new PayChannel();
         payChannel.setStatus(Integer.valueOf(PayChannelStatus.WAIT_HANDLE.value()));
         payChannel.setChannel(channel);
         String batchNo = RefundRequest.toBatchNo(DateTime.now().toDate(), refundTrackId);
         payChannel.setBatchNo(batchNo);
         payChannel.setTradeNo(oldPayChannel.getTradeNo());
         payChannel.setRefundOrderId(refundTrackId);
         payChannel.setPaymentCode(oldPayChannel.getPaymentCode());
         payChannel.setType(Integer.valueOf(PayChannelType.REFUND.value()));
         payChannel.setDescription("退款");
         payChannel.setOrderType(orderType);
         payChannel.setFee(refundAmount);
         payChannel.setBusinessType(Integer.valueOf(type.value()));
         payChannel.setIsCreatedDetail(Integer.valueOf(0));
         Response<PayChannel> payChannelRes = this.payWriteService.createPayChannelForRefund(payChannel);
         Preconditions.checkState(payChannelRes.isSuccess(), payChannelRes.getError());
         PayChannel existPayChannel = (PayChannel)payChannelRes.getResult();
         if(existPayChannel.getStatus().equals(Integer.valueOf(1))) {
            throw new ServiceException("can.not.duplicate.refund");
         }

         if(existPayChannel.getChannel().contains("alipay")) {
            AlipaySettingsDto alipaySettingsDto = this.paySettingsProvider.getAlipaySettings();
            String refundNotifyUrl = alipaySettingsDto.getRefundNotifyUrl();
            AlipayToken token = this.paySettingsProvider.buildAlipayToken();
            AlipayManager alipayManager = new AlipayManager();
            RedirectInfo info = alipayManager.refund(refundTrackId, reason, channel, tradeNo, existPayChannel.getBatchNo(), oldPayChannel.getPaymentCode(), refundAmount, RefundInterfaceType.NOPASSWRD.value(), refundNotifyUrl, token);
            if(info.isSuccess()) {
               return this.alipayRefund(info.getResult());
            }

            result.setError(info.getError());
         }

         if(existPayChannel.getChannel().contains("unionpay")) {
            String refundNotifyUrl = this.paySettingsProvider.getUnionSettings().getRefundNotifyUrl();
            UnionToken token = this.paySettingsProvider.buildUnionToken();
            UnionpayManager unionpayManager = new UnionpayManager();
            RedirectInfo info = unionpayManager.refund(refundTrackId, existPayChannel.getTradeNo(), existPayChannel.getChannel(), existPayChannel.getBatchNo(), oldPayChannel.getPaymentCode(), refundAmount, refundNotifyUrl, reason, token);
            if(info.isSuccess()) {
               return this.unionpayRefund(info.getResult());
            }

            result.setError(info.getError());
         }

         if(existPayChannel.getChannel().contains("kjtpay")) {
            String refundNotifyUrl = this.paySettingsProvider.getKjtSettings().getRefundNotifyUrl();
            KjtToken token = this.paySettingsProvider.buildKjtToken();
            KjtpayManager kjtpayManager = new KjtpayManager();
            RedirectInfo info = kjtpayManager.refund(refundTrackId, existPayChannel.getChannel(), existPayChannel.getTradeNo(), existPayChannel.getBatchNo(), oldPayChannel.getPaymentCode(), refundAmount, refundNotifyUrl, reason, token);
            if(info.isSuccess()) {
               return this.kjtpayRefund(info.getResult());
            }

            result.setError(info.getError());
         }

         if(existPayChannel.getChannel().contains("wechatpay")) {
            Response<TradePay> tradePayRes = this.payReadService.findTradePayByTradeNo(tradeNo);
            if(!tradePayRes.isSuccess()) {
               throw new ServiceException(tradePayRes.getError());
            }

            TradePay tradePay = (TradePay)tradePayRes.getResult();
            WechatpayManager wechatpayManager = new WechatpayManager();
            WxToken token = this.paySettingsProvider.buildWxToken();
            if(!this.payDebug.equals("true")) {
               WechatSettingsDto wechatSettingsDto = this.paySettingsProvider.getWechatSettings();
               String caFilePath = wechatSettingsDto.getCaFilePath();
               String certFilePath = wechatSettingsDto.getCertFilePath();
               RedirectInfo info = wechatpayManager.refund(refundTrackId, caFilePath, certFilePath, channel, existPayChannel.getTradeNo(), existPayChannel.getBatchNo(), oldPayChannel.getPaymentCode(), refundAmount, tradePay.getShouldFee(), token);
               if(!info.isSuccess()) {
                  result.setError(info.getError());
                  return result;
               }

               Response<Boolean> refundRes = this.wechatpayRefund(caFilePath, certFilePath, info.getResult(), token);
               if(!refundRes.isSuccess()) {
                  return refundRes;
               }
            } else {
               RedirectInfo info = wechatpayManager.mockRefund(refundTrackId, channel, existPayChannel.getTradeNo(), existPayChannel.getBatchNo(), oldPayChannel.getPaymentCode(), refundAmount, tradePay.getShouldFee(), token);
               if(!info.isSuccess()) {
                  result.setError(info.getError());
                  return result;
               }

               Response<Boolean> refundRes = this.mockWechatpayRefund(info.getResult());
               if(!refundRes.isSuccess()) {
                  return refundRes;
               }
            }

            return this.updateRefundSuccessForWechatRefund(existPayChannel.getBatchNo());
         }
      } catch (IllegalStateException var24) {
         log.error("refund reqeust where refundTrackId: {} fail,cause: {}", refundTrackId, var24.getMessage());
         result.setError(var24.getMessage());
      } catch (ServiceException var25) {
         log.error("refund reqeust where refundTrackId: {} fail,cause: {}", refundTrackId, var25.getMessage());
         result.setError(var25.getMessage());
      } catch (Exception var26) {
         log.error("refund reqeust where refundTrackId: {} fail,cause: {}", refundTrackId, Throwables.getStackTraceAsString(var26));
         result.setError("refund.reqeust.fail");
      }

      return result;
   }

   public Response alipayRefund(String url) {
      log.debug("refund url: {}", url);
      String body = HttpRequest.get(url).connectTimeout(10000).readTimeout(10000).body();
      log.debug("refund result: {}", body);
      return this.convertToResponse(body);
   }

   public Response unionpayRefund(String paramData) {
      Response<Boolean> result = new Response();
      Map<String, String> resmap = submitUrl(this.payDebug, paramData);
      if(((String)resmap.get("respCode")).equals("00")) {
         result.setResult(Boolean.TRUE);
         return result;
      } else {
         if(!((String)resmap.get("respCode")).equals("03") && !((String)resmap.get("respCode")).equals("04") && ((String)resmap.get("respCode")).equals("05")) {
            ;
         }

         log.info("请求报文=[" + paramData + "] <br/><br/>" + "同步应答报文=[" + resmap.toString() + "]");
         result.setResult(Boolean.FALSE);
         return result;
      }
   }

   public Response kjtpayRefund(String url) {
      Response<Boolean> result = new Response();
      log.info("kjtpay refund url: {}", url);
      String body = HttpRequest.get(url).connectTimeout(10000).readTimeout(10000).body();
      log.info("kjtpay refund result: {}", body);
      result.setResult(Boolean.TRUE);
      return result;
   }

   public Response wechatpayRefund(String caFilePath, String certFilePath, String preXml, WxToken token) {
      Response<Boolean> result = new Response();

      try {
         String body = HttpsUtil.build(caFilePath, certFilePath).callHttps(preXml, token.getMchId(), token.getRefundGateway()).getResContent();
         log.debug("wx order refund body: {}", body);
         WxPayRefundResponse payRefundReply = (WxPayRefundResponse)WxRequest.parse(body, WxPayRefundResponse.class);
         if(payRefundReply.isSuccess()) {
            result.setResult(Boolean.TRUE);
         } else {
            result.setError(payRefundReply.getErrorMsg());
         }
      } catch (Exception var8) {
         result.setError("wx.refund.fail");
         log.error("wx refund fail, cause: {}", Throwables.getStackTraceAsString(var8));
      }

      return result;
   }

   public Response mockWechatpayRefund(String url) {
      Response<Boolean> result = new Response();

      try {
         String body = HttpRequest.get(url).connectTimeout(10000).readTimeout(10000).body();
         log.debug("wx order mock refund body: {}", body);
         WxPayRefundResponse payRefundReply = (WxPayRefundResponse)WxRequest.parse(body, WxPayRefundResponse.class);
         if(payRefundReply.isSuccess()) {
            result.setResult(Boolean.TRUE);
         } else {
            result.setError(payRefundReply.getErrorMsg());
         }
      } catch (Exception var5) {
         result.setError("wx.mock.refund.fail");
         log.error("wx mock refund fail, cause: {}", Throwables.getStackTraceAsString(var5));
      }

      return result;
   }

   public static Map submitUrl(String payDebug, String requestUrl) {
      log.info("退款请求银联地址:" + requestUrl);
      String resultString = HttpRequest.get(requestUrl).connectTimeout(10000).readTimeout(10000).body();
      log.info("退款请求银联结果:" + resultString);
      Map<String, String> resData = new HashMap();
      if(null != resultString && !"".equals(resultString)) {
         resData = SDKUtil.convertResultStringToMap(resultString);
         if(!payDebug.equals("true")) {
            if(SDKUtil.validate(resData, "UTF-8")) {
               log.info("验证签名成功,可以继续后边的逻辑处理");
            } else {
               log.info("验证签名失败,必须验签签名通过才能继续后边的逻辑...");
            }
         }
      }

      return resData;
   }

   protected Response convertToResponse(String body) {
      Response<Boolean> result = new Response();

      try {
         Preconditions.checkState(!Strings.isNullOrEmpty(body), "alipay.refund.fail");
         AlipaySyncResponse refundResponse = (AlipaySyncResponse)xStream.fromXML(body);
         if(refundResponse.isSuccess()) {
            result.setResult(Boolean.TRUE);
         } else {
            log.error("refund raise fail: {}", refundResponse.getError());
            result.setError(refundResponse.getError());
         }
      } catch (IllegalStateException var4) {
         log.error("alipay refund request fail,error: {}", var4.getMessage());
         result.setError("alipay.refund.fail");
      } catch (Exception var5) {
         log.error("alipay refund request fail,cause: {}", Throwables.getStackTraceAsString(var5));
         result.setError("alipay.refund.fail");
      }

      return result;
   }

   private Response updateRefundSuccessForWechatRefund(String batchNo) {
      Response<Boolean> result = new Response();

      try {
         Response<PayChannel> payChannelR = this.payReadService.findPayChannelByBatchNoForRefund(batchNo);
         if(!payChannelR.isSuccess()) {
            log.warn("fail to find pay channel by batch no {} when refund success notify, error code:{}", batchNo, payChannelR.getError());
            throw new ServiceException(payChannelR.getError());
         }

         PayChannel payChannel = (PayChannel)payChannelR.getResult();
         PayChannel updatePayChannel = new PayChannel();
         updatePayChannel.setId(payChannel.getId());
         updatePayChannel.setStatus(Integer.valueOf(1));
         updatePayChannel.setRefundAt(new Date());
         Response<Boolean> updateStatusRes = this.payWriteService.updatePayChannel(updatePayChannel);
         if(!updateStatusRes.isSuccess()) {
            throw new ServiceException(updateStatusRes.getError());
         }

         Map<String, Object> updateParams = Maps.newHashMap();
         updateParams.put("orderId", payChannel.getRefundOrderId());
         updateParams.put("batchNo", batchNo);
         updateParams.put("channel", payChannel.getChannel());
         updateParams.put("tradeNo", payChannel.getTradeNo());
         this.eventBus.post(new RefundedEvent(updateParams, payChannel));
         result.setResult(Boolean.TRUE);
      } catch (ServiceException var8) {
         log.error("update refund success for wechat refund fail,where batchNo: {} ,error: {}", batchNo, var8.getMessage());
         result.setError("update.order.refund.success.fail");
      } catch (Exception var9) {
         log.error("update refund success for wechat refund fail,where batchNo: {} ,cause: {}", batchNo, Throwables.getStackTraceAsString(var9));
         result.setError("update.order.refund.success.fail");
      }

      return result;
   }

   public RedirectInfo payRequest(List orderIds, Integer orderType, String channel, Integer fee, String subject, Long buyerId) {
      return this.payRequest(orderIds, orderType, channel, fee, subject, buyerId, (String)null);
   }

   public RedirectInfo payRequest(List orderIds, Integer orderType, String channel, Integer fee, String subject, Long buyerId, String returnUrl) {
      String systemNo = this.generateNo((Long)orderIds.get(0), orderType);
      TradePay tradePay = this.createPayInfo(fee, buyerId, orderIds, orderType, systemNo, subject);
      Response<PayStage> stageResponse = this.payReadService.findRecentStageBySystemNo(tradePay.getSystemNo());
      if(!stageResponse.isSuccess()) {
         log.error("find recent pay stage fail,maybe current order already paid，Please use systemNo:{} to check pay_stage and pay_channel", tradePay.getSystemNo());
         throw new ServiceException(stageResponse.getError());
      } else {
         PayStage stage = (PayStage)stageResponse.getResult();
         PayChannel payChannel = new PayChannel();
         payChannel.setStatus(Integer.valueOf(0));
         payChannel.setType(Integer.valueOf(PayChannelType.PAID.value()));
         payChannel.setStageId(stage.getId());
         payChannel.setChannel(channel);
         payChannel.setFee(stage.getFee());
         payChannel.setOrderType(orderType);
         payChannel.setTradeNo(this.generateNo((Long)orderIds.get(0), orderType));
         payChannel.setCreatedAt(new Date());
         payChannel.setUpdatedAt(new Date());
         payChannel.setIsCreatedDetail(Integer.valueOf(0));
         payChannel.setBusinessType(Integer.valueOf(PayChannelBusinessType.PAID.value()));
         Response<PayChannel> payChannelRes = this.payWriteService.createPayChannel(payChannel);
         if(!payChannelRes.isSuccess()) {
            throw new ServiceException(payChannelRes.getError());
         } else {
            PayChannel existPaychannel = (PayChannel)payChannelRes.getResult();
            if(existPaychannel.getStatus().equals(Integer.valueOf(1))) {
               throw new ServiceException("can.not.duplicate.pay");
            } else {
               return channel.contains("alipay")?this.getAlipayRedirectInfo(channel, subject, stage.getContent(), existPaychannel.getTradeNo(), systemNo, payChannel.getFee(), returnUrl):(channel.contains("unionpay")?this.getUnionpayRedirectInfo(channel, existPaychannel.getTradeNo(), systemNo, payChannel.getFee(), returnUrl):(channel.contains("kjtpay")?this.getKjtpayRedirectInfo(channel, subject, stage.getContent(), existPaychannel.getTradeNo(), systemNo, payChannel.getFee(), returnUrl):(channel.contains("wechatpay")?this.getWechatpayRedirectInfo(channel, subject, stage.getContent(), existPaychannel.getTradeNo(), fee, buyerId):null)));
            }
         }
      }
   }

   private RedirectInfo getAlipayRedirectInfo(String channel, String subject, String content, String tradNo, String systemNo, Integer fee, String returnUrl) {
      AlipayManager manager = new AlipayManager();
      Date expiredAt = (new DateTime()).plusDays(3).minusHours(3).toDate();
      AlipaySettingsDto alipaySettingsDto = this.paySettingsProvider.getAlipaySettings();
      String notifyUrl = alipaySettingsDto.getNotifyUrl();
      if(Strings.isNullOrEmpty(returnUrl)) {
         returnUrl = alipaySettingsDto.getReturnUrl();
      }

      AlipayToken token = this.paySettingsProvider.buildAlipayToken();
      RedirectInfo info = manager.pay(channel, subject, content, tradNo, systemNo, expiredAt, fee, notifyUrl, returnUrl, token);
      if(!info.isSuccess()) {
         throw new ServiceException(info.getError());
      } else {
         return info;
      }
   }

   private RedirectInfo getUnionpayRedirectInfo(String channel, String tradeNo, String systemNo, Integer fee, String returnUrl) {
      UnionpayManager unionpayManager = new UnionpayManager();
      UnionSettingsDto unionSettingsDto = this.paySettingsProvider.getUnionSettings();
      String notifyUrl = unionSettingsDto.getNotifyUrl();
      if(Strings.isNullOrEmpty(returnUrl)) {
         returnUrl = unionSettingsDto.getReturnUrl();
      }

      UnionToken token = this.paySettingsProvider.buildUnionToken();
      SDKConfig config = new SDKConfig();
      config.setSignCertPath(unionSettingsDto.getCertPath());
      config.setSignCertPwd(unionSettingsDto.getCertPwd());
      config.setSignCertType(unionSettingsDto.getCertType());
      config.setValidateCertDir(unionSettingsDto.getValidateCertDir());
      config.setEncryptCertPath(unionSettingsDto.getEncryptCertPath());
      config.setSingleMode(unionSettingsDto.getSingleMode());
      config.setTransFilePatch(unionSettingsDto.getTransFilePatch());
      RedirectInfo info = unionpayManager.pay(channel, tradeNo, systemNo, fee, notifyUrl, returnUrl, token);
      if(!info.isSuccess()) {
         throw new ServiceException(info.getError());
      } else {
         return info;
      }
   }

   private RedirectInfo getKjtpayRedirectInfo(String channel, String subject, String content, String tradeNo, String systemNo, Integer fee, String returnUrl) {
      KjtpayManager kjtpayManager = new KjtpayManager();
      KjtSettingsDto kjtSettingsDto = this.paySettingsProvider.getKjtSettings();
      String notifyUrl = kjtSettingsDto.getNotifyUrl();
      if(Strings.isNullOrEmpty(returnUrl)) {
         returnUrl = kjtSettingsDto.getReturnUrl();
      }

      KjtToken token = this.paySettingsProvider.buildKjtToken();
      RedirectInfo info = kjtpayManager.pay(channel, subject, content, tradeNo, systemNo, (Date)null, fee, notifyUrl, returnUrl, token);
      if(!info.isSuccess()) {
         throw new ServiceException(info.getError());
      } else {
         return info;
      }
   }

   private RedirectInfo getWechatpayRedirectInfo(String channel, String subject, String content, String tradeNo, Integer fee, Long buyerId) {
      RedirectInfo redirect = new RedirectInfo();
      WechatpayManager wechatpayManager = new WechatpayManager();
      WechatSettingsDto wechatSettingsDto = this.paySettingsProvider.getWechatSettings();
      String notifyUrl = wechatSettingsDto.getNotifyUrl();
      WxToken token = this.paySettingsProvider.buildWxToken();
      if(!this.payDebug.equals("true")) {
         RedirectInfo prePayInfo = wechatpayManager.prePay(channel, subject, content, tradeNo, fee, notifyUrl, buyerId, token);
         if(!prePayInfo.isSuccess()) {
            log.error("generate pre pay info fail where channel:{} tradeNo:{},error:{}", new Object[]{channel, tradeNo, prePayInfo.getError()});
            throw new ServiceException(prePayInfo.getError());
         } else {
            Response<WxPrePayResponse> response = this.wxPrePay(prePayInfo.getResult(), token);
            if(!response.isSuccess()) {
               log.error("pre pay post fail where channel:{} tradeNo:{},error:{}", new Object[]{channel, tradeNo, response.getError()});
               throw new ServiceException(response.getError());
            } else {
               RedirectInfo payInfo = wechatpayManager.pay(channel, tradeNo, (WxPrePayResponse)response.getResult(), token);
               if(!payInfo.isSuccess()) {
                  log.error("generate pay info fail where channel:{} tradeNo:{},error:{}", new Object[]{channel, tradeNo, payInfo.getError()});
                  throw new ServiceException(payInfo.getError());
               } else {
                  if(channel.endsWith("jsapi")) {
                     redirect.setIsRedirectNow(Boolean.FALSE);
                     redirect.setChannel(channel);
                     redirect.setResult(payInfo.getResult());
                  } else {
                     HashMap payContext = (HashMap)JsonMapper.nonEmptyMapper().fromJson(payInfo.getResult(), HashMap.class);
                     File file = (new QrHelper()).getQrCode(((WxPrePayResponse)response.getResult()).getCodeUrl());
                     String qrcodePicUrl = this.imageServer.write("/" + buyerId.toString() + "/" + FileUtil.newFileName(file.getName()), file);
                     String payView = "http://" + ThreadVars.getHost() + "/pay/mobile-pay";
                     payView = payView + "?qrcodeUrl=" + wechatSettingsDto.getImageBaseUrl() + qrcodePicUrl + "&tradeNo=" + payContext.get("tradeNo");
                     redirect.setIsRedirectNow(Boolean.TRUE);
                     redirect.setChannel(channel);
                     redirect.setResult(payView);
                  }

                  return redirect;
               }
            }
         }
      } else {
         RedirectInfo prePayInfo = wechatpayManager.mockPay(channel, subject, content, tradeNo, fee, notifyUrl, buyerId, token);
         return prePayInfo;
      }
   }

   public Response wxPrePay(String preXml, WxToken token) {
      Response<WxPrePayResponse> result = new Response();
      log.debug("wx prepay url: {}", token.getGateway());
      log.debug("wx prepay pre xml: {}", preXml);
      String body = HttpRequest.post(token.getGateway()).send(preXml).trustAllCerts().trustAllHosts().body();
      log.debug("wx prepay body: {}", body);
      if(WxRequest.verify(body, token)) {
         WxPrePayResponse wxPrePayResponse = (WxPrePayResponse)WxRequest.parse(body, WxPrePayResponse.class);
         result.setResult(wxPrePayResponse);
      }

      return result;
   }

   private void checkParam(Map map) {
      if(!map.containsKey("orderId")) {
         throw new ServiceException("prepay.action.param.order.ids.invalid");
      } else if(!map.containsKey("channel")) {
         throw new ServiceException("prepay.action.param.channel.invalid");
      }
   }

   private TradePay createPayInfo(Integer fee, Long buyerId, List ids, Integer orderType, String systemNo, String subject) {
      String orderIds = Joiners.COMMA.join(ids);
      Response<TradePay> existTradePayRes = this.payReadService.checkNeedCreateTradePay(orderIds, ids.size() > 1?Boolean.TRUE:Boolean.FALSE, orderType);
      if(!existTradePayRes.isSuccess()) {
         throw new ServiceException(existTradePayRes.getError());
      } else if(Arguments.notNull(existTradePayRes.getResult())) {
         return (TradePay)existTradePayRes.getResult();
      } else {
         List<PayStageInfo> stageInfos = Lists.newArrayList();
         PayStageInfo info = new PayStageInfo();
         info.setCurrentStage(Integer.valueOf(1));
         info.setFee(fee);
         stageInfos.add(info);
         if(Arguments.isNullOrEmpty(stageInfos)) {
            throw new ServiceException("stage.info.invalid");
         } else {
            TradePay tradePay = new TradePay();
            tradePay.setBuyerId(buyerId);
            tradePay.setMergePaid(ids.size() > 1?Boolean.TRUE:Boolean.FALSE);
            tradePay.setShouldFee(this.getMultiOrderFee(stageInfos));
            tradePay.setAlreadyFee(Integer.valueOf(0));
            tradePay.setOrderId(ids.size() > 1?null:(Long)ids.get(0));
            tradePay.setOrderIds(orderIds);
            tradePay.setOrderType(orderType);
            tradePay.setPaidStatus(Integer.valueOf(0));
            tradePay.setCurrentStage(Integer.valueOf(0));
            tradePay.setPaidType(Integer.valueOf(1));
            tradePay.setStage(Integer.valueOf(stageInfos.size()));
            tradePay.setSubject(subject);
            tradePay.setSystemNo(systemNo);
            tradePay.setCreatedAt(new Date());
            tradePay.setUpdatedAt(new Date());
            Response<Boolean> tradePayRes = this.payWriteService.createTradePayWithStage(tradePay, stageInfos, (Date)null);
            if(!tradePayRes.isSuccess()) {
               throw new ServiceException(tradePayRes.getError());
            } else {
               return tradePay;
            }
         }
      }
   }

   private Integer getMultiOrderFee(List infos) {
      int total = 0;

      for(PayStageInfo info : infos) {
         total += info.getFee().intValue();
      }

      return Integer.valueOf(total);
   }

   private String generateNo(Long orderId, Integer orderType) {
      String prefix = DFT.print(DateTime.now());
      String suffix = orderId.toString();
      Integer orderLenght = Integer.valueOf(suffix.length());
      String random = GenerateRandom.rand(15 - orderLenght.intValue());
      return prefix + orderType + random + suffix;
   }

   static {
      xStream.autodetectAnnotations(true);
      xStream.processAnnotations(AlipaySyncResponse.class);
   }
}
