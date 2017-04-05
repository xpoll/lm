package io.terminus.parana.web.mock.pay.manager;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Joiner;
import com.google.common.base.Objects;
import com.google.common.base.Preconditions;
import com.google.common.base.Splitter;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;
import com.itrus.cryptorole.bc.SenderBcImpl;
import io.terminus.common.utils.Arguments;
import io.terminus.parana.pay.mock.model.MockKjtpayTrans;
import io.terminus.parana.pay.mock.service.MockPayWriteService;
import io.terminus.parana.web.mock.pay.annotations.MockPay;
import io.terminus.parana.web.mock.pay.manager.AbstractMockGatewayManager;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Date;
import java.util.List;
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

@Component
@MockPay(
   channels = {"kjtpay", "mock-kjtpay"}
)
public class KjtpayMockGatewayManager extends AbstractMockGatewayManager {
   private static final Logger log = LoggerFactory.getLogger(KjtpayMockGatewayManager.class);
   private DateTimeFormatter DATE = DateTimeFormat.forPattern("yyyyMMddHHmmss");
   @Autowired
   private MockPayWriteService mockPayWriteService;

   public String serve(HttpServletRequest request, HttpServletResponse response) {
      String service = request.getParameter("service");
      if(Objects.equal(service, "create_instant_pay")) {
         String trade_list = request.getParameter("trade_list");
         List<String> items = Splitter.on("~").splitToList(trade_list);
         String fee = (String)items.get(2);
         request.setAttribute("total_fee", fee);
         log.info("[Kjtpay Server]Received pay request, then forward:/mock/pay...");
         return "forward:/mock/kjtpay";
      } else {
         if(Objects.equal(service, "create_refund")) {
            log.info("[Kjtpay Server]No password refund");
            this.refund(request);
         }

         return null;
      }
   }

   public Map payConfirm(HttpServletRequest request) {
      log.info("Mocked kjtpay context:[{}]", request.getParameterMap());
      String outerNo = request.getParameter("requestNo");
      String fee = request.getParameter("fee");
      Map<String, String> params = Maps.newTreeMap();
      params.put("notify_id", this.getRandomNo());
      params.put("notify_type", "trade_status_sync");
      params.put("notify_time", this.DATE.print(DateTime.now()));
      params.put("_input_charset", "UTF-8");
      params.put("version", "1.0");
      params.put("outer_trade_no", outerNo);
      params.put("inner_trade_no", this.getRandomNo());
      params.put("trade_status", "TRADE_FINISHED");
      params.put("trade_amount", fee);
      params.put("gmt_create", this.DATE.print(DateTime.now()));
      params.put("gmt_payment", this.DATE.print(DateTime.now()));
      params.put("gmt_close", "");
      String pfxPath = request.getParameter("pfxPath");
      String keyPassword = request.getParameter("keyPassword");
      String cerPath = request.getParameter("cerPath");
      params.put("sign", "sign");
      params.put("sign_type", "ITRUSSRV");

      try {
         params.put("trade_amount", URLEncoder.encode(((String)params.get("trade_amount")).toString(), "UTF-8"));
      } catch (UnsupportedEncodingException var9) {
         log.error("kjtpay mock fail,cause: {}", Throwables.getStackTraceAsString(var9));
      }

      List<String> items = Splitter.on("~").splitToList(fee);
      this.createMockedKjtpayTrans(params, items);
      return params;
   }

   public void refund(HttpServletRequest request) {
      String outerNo = request.getParameter("outer_trade_no");
      String origNo = request.getParameter("orig_outer_trade_no");
      String refundAmount = request.getParameter("refund_amount");
      Map<String, String> params = Maps.newTreeMap();
      params.put("refund_status", "REFUND_SUCCESS");
      params.put("inner_trade_no", this.getRandomNo());
      params.put("outer_trade_no", outerNo);
      params.put("orig_outer_trade_no", origNo);
      params.put("flage", "test");
      String notifyUrl = request.getParameter("notify_url");
      String suffix = Joiner.on('&').withKeyValueSeparator("=").join(params);
      HttpRequest.get(notifyUrl + "?" + suffix).connectTimeout(1000000).readTimeout(1000000).body();
      this.createRefundTrans(params, refundAmount);
   }

   private void createMockedKjtpayTrans(Map map, List items) {
      Preconditions.checkState("TRADE_FINISHED".equals(map.get("trade_status")), "query.kjtpay.trans.fail");
      this.mockPayWriteService.createMockKjtpayTrans(this.initTrans(map, items));
   }

   public MockKjtpayTrans initTrans(Map params, List items) {
      MockKjtpayTrans kjtpayTrans = new MockKjtpayTrans();
      kjtpayTrans.setAmount((String)items.get(2));
      kjtpayTrans.setInnerNo((String)params.get("inner_trade_no"));
      kjtpayTrans.setOuterNo((String)params.get("outer_trade_no"));
      kjtpayTrans.setOrigOuterNo((String)params.get("orig_outer_trade_no"));
      kjtpayTrans.setType((String)items.get(3));
      kjtpayTrans.setOrderAt(new Date());
      kjtpayTrans.setPaidAt(new Date());
      kjtpayTrans.setRate("10");
      kjtpayTrans.setRateFee(this.getRateFee(kjtpayTrans.getAmount()));
      kjtpayTrans.setStatus((String)items.get(11));
      kjtpayTrans.setCreatedAt(new Date());
      kjtpayTrans.setUpdatedAt(new Date());
      return kjtpayTrans;
   }

   public void createRefundTrans(Map params, String refundAmount) {
      MockKjtpayTrans kjtpayTrans = new MockKjtpayTrans();
      kjtpayTrans.setAmount(refundAmount);
      kjtpayTrans.setInnerNo((String)params.get("inner_trade_no"));
      kjtpayTrans.setOuterNo((String)params.get("outer_trade_no"));
      kjtpayTrans.setOrigOuterNo((String)params.get("orig_outer_trade_no"));
      kjtpayTrans.setType("3");
      kjtpayTrans.setOrderAt(new Date());
      kjtpayTrans.setPaidAt(new Date());
      kjtpayTrans.setRate("10");
      kjtpayTrans.setRateFee(this.getRateFee(kjtpayTrans.getAmount()));
      kjtpayTrans.setStatus("951");
      kjtpayTrans.setCreatedAt(new Date());
      kjtpayTrans.setUpdatedAt(new Date());
      this.mockPayWriteService.createMockKjtpayTrans(kjtpayTrans);
   }

   private String getRateFee(String rf) {
      Double rateFee = Double.valueOf(Double.valueOf(rf).doubleValue() / 100.0D);
      return rateFee.toString();
   }

   private String getRandomNo() {
      String prefix = this.DATE.print(DateTime.now());
      String suffix = "0000000" + (new Random()).nextInt(100000);
      suffix = suffix.substring(suffix.length() - 7, suffix.length());
      return prefix + "0" + suffix;
   }

   public void sign(Map params, String pfxPath, String keyPassword, String cerPath) {
      try {
         Map<String, String> param = paraFilter(params);
         String toVerify = Joiner.on('&').withKeyValueSeparator("=").join(param);
         SenderBcImpl send = new SenderBcImpl();
         send.initCertWithKey(pfxPath, keyPassword);
         SenderBcImpl sender = new SenderBcImpl();
         SenderBcImpl recipient = new SenderBcImpl();
         sender.initCertWithKey(pfxPath, keyPassword);
         recipient.initCertWithKey(pfxPath, keyPassword);
         InputStream streamCert = new FileInputStream(cerPath);
         CertificateFactory factory = CertificateFactory.getInstance("X.509");
         X509Certificate X509Cert = (X509Certificate)factory.generateCertificate(streamCert);
         sender.addRecipientCert(X509Cert);
         String sign = sender.signMessage(toVerify);
         params.put("sign", sign);
         params.put("sign_type", "ITRUSSRV");
         this.encoderSign(params);
         this.encoderTradeList(params);
      } catch (Exception var14) {
         throw new RuntimeException(var14);
      }
   }

   public void encoderTradeList(Map params) {
      String content = (String)params.get("trade_list");
      if(Arguments.notEmpty(content)) {
         try {
            params.put("trade_list", URLEncoder.encode(content, "UTF-8"));
         } catch (UnsupportedEncodingException var4) {
            var4.printStackTrace();
         }
      }

   }

   public void encoderSign(Map params) {
      String sign = (String)params.get("sign");
      if(Arguments.notEmpty(sign)) {
         try {
            params.put("sign", URLEncoder.encode(sign, "UTF-8"));
         } catch (UnsupportedEncodingException var4) {
            var4.printStackTrace();
         }
      }

   }

   public static Map paraFilter(Map param) {
      Map<String, String> result = Maps.newTreeMap();
      if(param != null && param.size() > 0) {
         for(String key : param.keySet()) {
            String value = (String)param.get(key);
            if(value != null && !value.equals("") && !key.equalsIgnoreCase("sign") && !key.equalsIgnoreCase("sign_type")) {
               result.put(key, value);
            }
         }

         return result;
      } else {
         return result;
      }
   }
}
