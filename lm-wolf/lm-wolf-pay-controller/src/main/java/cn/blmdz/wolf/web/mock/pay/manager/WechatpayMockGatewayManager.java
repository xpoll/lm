package cn.blmdz.wolf.web.mock.pay.manager;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.Date;
import java.util.Map;
import java.util.Random;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;
import com.google.common.hash.HashFunction;
import com.google.common.hash.Hashing;
import com.google.common.io.BaseEncoding;
import com.thoughtworks.xstream.XStream;

import cn.blmdz.aide.pay.channel.wechatpay.dto.WxPayNotifyDto;
import cn.blmdz.aide.pay.channel.wechatpay.dto.WxPayRefundResponse;
import cn.blmdz.aide.pay.utils.XmlUtils;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.NumberUtils;
import cn.blmdz.wolf.pay.mock.model.MockWechatpayTrans;
import cn.blmdz.wolf.pay.mock.service.MockPayWriteService;
import cn.blmdz.wolf.web.mock.pay.annotations.MockPay;

@Component
@MockPay(
   channels = {"wechatpay", "wechatpay-jsapi", "mock-wechatpay-jsapi"}
)
public class WechatpayMockGatewayManager extends AbstractMockGatewayManager {
   private static final Logger log = LoggerFactory.getLogger(WechatpayMockGatewayManager.class);
   private DateTimeFormatter DFT_TIME = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");
   private DateTimeFormatter DATE = DateTimeFormat.forPattern("yyyyMMddHHmmss");
   private static final HashFunction MD5 = Hashing.md5();
   private static final Charset UTF8 = Charset.forName("UTF-8");
   private static final XStream xstream = new XStream();
   @Autowired(
      required = false
   )
   private MockPayWriteService payWriteService;

   public String serve(HttpServletRequest request, HttpServletResponse response) {
      String refundFee = request.getParameter("refund_fee");
      if(!Strings.isNullOrEmpty(refundFee)) {
         try {
            log.info("[wxpay Server]No password refund");
            response.getWriter().write(this.refund(request));
         } catch (IOException var5) {
            log.error("sys error,cause:{}", Throwables.getStackTraceAsString(var5));
         }

         return null;
      } else {
         log.info("start handle wechat pay with mock gateway");
         return "forward:/mock/wechatpay";
      }
   }

   public Map payConfirm(HttpServletRequest request) {
      Map<String, String> map = Maps.newHashMap();

      try {
         log.info("Mocked wechatpay context:[{}]", request.getParameterMap());
         String appid = request.getParameter("appid");
         String mchId = request.getParameter("mchId");
         String openId = request.getParameter("openId");
         String orderId = request.getParameter("orderId");
         WxPayNotifyDto dto = new WxPayNotifyDto();
         dto.setAppid(appid);
         dto.setOutTradeNo(orderId);
         dto.setMchId(mchId);
         dto.setOpenid(openId);
         dto.setResultCode("SUCCESS");
         dto.setReturnCode("SUCCESS");
         dto.setTransactionId(this.getRandomNo());
         this.createWechatPayTrans(dto, request);
         String dtoXml = xstream.toXML(dto);
         Map params = XmlUtils.fromXML(dtoXml);
         String paternerkey = request.getParameter("paternerkey");
         String toVerify = Joiner.on('&').withKeyValueSeparator("=").join(params) + "&key=" + paternerkey;
         String sign = BaseEncoding.base16().encode(MD5.newHasher().putString(toVerify, UTF8).hash().asBytes()).toUpperCase();
         map.put("sign", sign);
         map.put("sendXml", xstream.toXML(dto));
      } catch (Exception var13) {
         log.error("mock wechat pay failed, cause:{}", Throwables.getStackTraceAsString(var13));
      }

      return map;
   }

   public String refund(HttpServletRequest request) {
      WxPayRefundResponse dto = new WxPayRefundResponse();

      try {
         log.info("Mocked wechatpay refund context:[{}]", request.getParameterMap());
         String appid = request.getParameter("appid");
         String mchId = request.getParameter("mchId");
         String orderId = request.getParameter("out_trade_no");
         String transactionId = request.getParameter("transaction_id");
         String outRefundNo = request.getParameter("out_refund_no");
         String refundFee = request.getParameter("refund_fee");
         String totalFee = request.getParameter("total_fee");
         dto.setTransactionId(transactionId);
         dto.setOutTradeNo(orderId);
         dto.setOutRefundNo(outRefundNo);
         dto.setRefundFee(Integer.valueOf(refundFee));
         dto.setAppid(appid);
         dto.setOutTradeNo(orderId);
         dto.setMchId(mchId);
         dto.setResultCode("SUCCESS");
         dto.setReturnCode("SUCCESS");
         this.createWechatPayRefundTrans(dto, request, totalFee);
         return xstream.toXML(dto);
      } catch (Exception var10) {
         log.error("mock wechat pay failed, cause:{}", Throwables.getStackTraceAsString(var10));
         dto.setResultCode("FAIL");
         dto.setReturnCode("FAIL");
         dto.setReturnMsg("mock.refund.fail");
         return xstream.toXML(dto);
      }
   }

   private String getRandomNo() {
      String prefix = this.DATE.print(DateTime.now());
      String suffix = "0000000" + (new Random()).nextInt(100000);
      suffix = suffix.substring(suffix.length() - 7, suffix.length());
      return prefix + "0" + suffix;
   }

   private void createWechatPayTrans(WxPayNotifyDto dto, HttpServletRequest request) throws UnsupportedEncodingException {
      MockWechatpayTrans trans = new MockWechatpayTrans();
      trans.setTransactionId(dto.getTransactionId());
      trans.setOutTradeNo(dto.getOutTradeNo());
      trans.setTradeStatus("SUCCESS");
      trans.setTradeTime(this.DFT_TIME.print(DateTime.now()));
      trans.setTradeType("NATIVE");
      trans.setAppid(dto.getAppid());
      trans.setMchId(dto.getMchId());
      trans.setOpenId(dto.getOpenid());
      trans.setBankType("CMB_CREDIT");
      trans.setFeeType("CNY");
      trans.setTotalFee(request.getParameter("fee"));
      trans.setCouponFee("0.00");
      trans.setBody(request.getParameter("body"));
      trans.setAttach("GROUP");
      trans.setRate("0.60%");
      trans.setPoundageFee((new BigDecimal(trans.getTotalFee())).multiply(new BigDecimal("0.006")).toString());
      trans.setTradeInfo("mock支付账务");
      trans.setTradeAt(new Date());
      Response<Boolean> res = this.payWriteService.createMockWechatPayTrans(trans, Integer.valueOf(1));
      Preconditions.checkState(res.isSuccess(), res.getError());
      log.info("create wechat pay trans success");
   }

   private void createWechatPayRefundTrans(WxPayRefundResponse dto, HttpServletRequest request, String totalFee) throws UnsupportedEncodingException {
      MockWechatpayTrans trans = new MockWechatpayTrans();
      trans.setTransactionId(dto.getTransactionId());
      trans.setOutTradeNo(dto.getOutTradeNo());
      trans.setTradeStatus("REFUND");
      trans.setTradeTime(this.DFT_TIME.print(DateTime.now()));
      trans.setTradeType("NATIVE");
      trans.setAppid(dto.getAppid());
      trans.setMchId(dto.getMchId());
      trans.setOpenId("osvRcuJffD-1PgTnT34jOTDR8NJI");
      trans.setBankType("CMB_CREDIT");
      trans.setFeeType("CNY");
      trans.setTotalFee("");
      trans.setRefundFee(NumberUtils.formatPrice(dto.getRefundFee()).toString());
      trans.setRefundStatus("SUCCESS");
      trans.setRefundId(this.getRandomNo());
      trans.setOutRefundNo(dto.getOutRefundNo());
      trans.setCouponFee("0.00");
      trans.setBody(request.getParameter("body"));
      trans.setAttach("GROUP");
      trans.setRate("0.60%");
      trans.setPoundageFee((new BigDecimal(NumberUtils.formatPrice(Long.valueOf(totalFee)))).multiply(new BigDecimal("0.006")).toString());
      trans.setTradeInfo("mock支付账务退款");
      trans.setTradeAt(new Date());
      Response<Boolean> res = this.payWriteService.createMockWechatPayTrans(trans, Integer.valueOf(-1));
      Preconditions.checkState(res.isSuccess(), res.getError());
      log.info("create wechat pay trans success");
   }

   static {
      xstream.autodetectAnnotations(true);
      xstream.processAnnotations(WxPayNotifyDto.class);
   }
}
