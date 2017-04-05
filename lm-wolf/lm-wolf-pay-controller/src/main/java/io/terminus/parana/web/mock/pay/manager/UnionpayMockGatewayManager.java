package io.terminus.parana.web.mock.pay.manager;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Joiner;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;
import io.terminus.lib.pay.channel.unionpay.request.Request;
import io.terminus.parana.pay.mock.model.MockUnionPayTrans;
import io.terminus.parana.pay.mock.service.MockPayWriteService;
import io.terminus.parana.web.mock.pay.annotations.MockPay;
import io.terminus.parana.web.mock.pay.manager.AbstractMockGatewayManager;
import java.io.IOException;
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

@Component
@MockPay(
   channels = {"unionpay", "mock-unionpay"}
)
public class UnionpayMockGatewayManager extends AbstractMockGatewayManager {
   private static final Logger log = LoggerFactory.getLogger(UnionpayMockGatewayManager.class);
   @Autowired
   private MockPayWriteService payWriteService;
   private DateTimeFormatter DATE = DateTimeFormat.forPattern("yyyyMMddHHmmss");
   private DateTimeFormatter DATE1 = DateTimeFormat.forPattern("MMddHHmmss");
   private DateTimeFormatter DATE2 = DateTimeFormat.forPattern("MMdd");
   protected static final String ENCODING = "UTF-8";

   public String serve(HttpServletRequest request, HttpServletResponse response) {
      String origQryId = request.getParameter("origQryId");
      if(!Strings.isNullOrEmpty(origQryId)) {
         try {
            log.info("[Unionpay Server]No password refund");
            response.getWriter().write(this.refund(request));
         } catch (IOException var5) {
            log.error("sys error,cause:{}", Throwables.getStackTraceAsString(var5));
         }

         return null;
      } else {
         log.info("start handle union pay with mock gateway");
         return "forward:/mock/unionpay";
      }
   }

   public Map payConfirm(HttpServletRequest request) {
      log.info("Mocked unionpay context:[{}]", request.getParameterMap());
      String outerNo = request.getParameter("requestNo");
      String fee = request.getParameter("fee");
      String merId = request.getParameter("merId");
      Map<String, String> params = Maps.newTreeMap();
      params.put("signMethod", "01");
      params.put("currencyCode", "156");
      params.put("queryId", this.getRandomNo());
      params.put("orderId", outerNo);
      params.put("traceNo", this.getRandomNo());
      params.put("txnAmt", fee);
      params.put("txnTime", this.DATE.print(DateTime.now()));
      params.put("traceTime", this.DATE2.print(DateTime.now()));
      params.put("settleCurrencyCode", "156");
      params.put("settleAmt", fee);
      params.put("respCode", "00");
      params.put("respMsg", "SUCCESS");
      params.put("merId", merId);
      params.put("encoding", "UTF-8");
      Map<String, String> submitFromData = Request.signData(params);
      this.createMockUnionpayTrans(submitFromData);
      return submitFromData;
   }

   public void createMockUnionpayTrans(Map params) {
      MockUnionPayTrans trans = new MockUnionPayTrans();
      trans.setTransactionCode("S56");
      trans.setAcqInsCode("48021000");
      trans.setSendCode("00049993");
      trans.setTraceNo((String)params.get("traceNo"));
      trans.setPayCardNo("621626*********0018");
      trans.setTxnAmt((String)params.get("txnAmt"));
      trans.setMerCatCode("5722");
      trans.setTermType("07");
      trans.setQueryId((String)params.get("queryId"));
      trans.setType("01");
      trans.setOrderId((String)params.get("orderId"));
      trans.setPayCardType("01");
      trans.setOriginalTraceNo("");
      trans.setOriginalTime("");
      trans.setThirdPartyFee("389");
      trans.setSettleAmount("C000000051511");
      trans.setPayType("0001");
      trans.setCompanyCode("");
      trans.setTxnType("01");
      trans.setTxnSubType("");
      trans.setBizType("000201");
      trans.setAccType("");
      trans.setBillType("");
      trans.setBillNo("");
      trans.setInteractMode("");
      trans.setOrigQryId("");
      trans.setMerId((String)params.get("merId"));
      trans.setDivideType("");
      trans.setSubMerId("000000000000000");
      trans.setSubMerAbbr("");
      trans.setDivideAmount("000000000000");
      trans.setClearing("C000000051511");
      trans.setTermId("01080209");
      trans.setMerReserved("201511121751461000000000000092");
      trans.setDiscount("0000");
      trans.setInvoice("00");
      trans.setAdditionThirdPartyFee("00");
      trans.setStage("");
      trans.setTransactionMedia("7");
      trans.setOriginalOrderId("");
      trans.setTxnTime(new Date());
      trans.setCreatedAt(new Date());
      trans.setUpdatedAt(new Date());
      this.payWriteService.createMockUnionpayTrans(trans);
   }

   private String getRandomNo() {
      String prefix = this.DATE.print(DateTime.now());
      String suffix = "0000000" + (new Random()).nextInt(100000);
      suffix = suffix.substring(suffix.length() - 7, suffix.length());
      return prefix + "0" + suffix;
   }

   public String refund(HttpServletRequest request) {
      log.info("Mocked unionpay refund context:[{}]", request.getParameterMap());
      String batchNo = request.getParameter("orderId");
      String fee = request.getParameter("txnAmt");
      String certId = request.getParameter("certId");
      String origQryId = request.getParameter("origQryId");
      Map<String, String> params = Maps.newTreeMap();
      params.put("certId", certId);
      params.put("signMethod", "01");
      params.put("currencyCode", "156");
      params.put("queryId", this.getRandomNo());
      params.put("orderId", batchNo);
      params.put("traceNo", this.getRandomNo());
      params.put("txnAmt", fee);
      params.put("txnTime", this.DATE.print(DateTime.now()));
      params.put("traceTime", this.DATE2.print(DateTime.now()));
      params.put("settleCurrencyCode", "156");
      params.put("settleAmt", fee);
      params.put("respCode", "00");
      params.put("respMsg", "SUCCESS");
      params.put("origQryId", origQryId);
      params.put("encoding", "UTF-8");
      Map<String, String> submitFromData = Request.signData(params);
      String suffix = Joiner.on('&').withKeyValueSeparator("=").join(submitFromData);
      String url = request.getParameter("backUrl");
      String result = HttpRequest.get(url + "?" + suffix).connectTimeout(10000).readTimeout(10000).body();
      log.info("[Unionpay Server] Async refund result notify \n\t url:   [{}]\n\t params:[{}]\n\t result:[{}]", new Object[]{url, params, result});
      Map<String, String> resultData = Maps.newHashMap();
      resultData.put("respCode", "00");
      this.createMockUnionpayRefundTrans(params);
      return Joiner.on('&').withKeyValueSeparator("=").join(resultData);
   }

   public void createMockUnionpayRefundTrans(Map params) {
      MockUnionPayTrans trans = new MockUnionPayTrans();
      trans.setTransactionCode("S30");
      trans.setAcqInsCode("48021000");
      trans.setSendCode("00049993");
      trans.setTraceNo((String)params.get("traceNo"));
      trans.setPayCardNo("621626*********0018");
      trans.setTxnAmt((String)params.get("txnAmt"));
      trans.setMerCatCode("5722");
      trans.setTermType("07");
      trans.setQueryId((String)params.get("queryId"));
      trans.setType("01");
      trans.setOrderId((String)params.get("orderId"));
      trans.setPayCardType("01");
      trans.setOriginalTraceNo("");
      trans.setOriginalTime("");
      trans.setThirdPartyFee("389");
      trans.setSettleAmount("C000000051511");
      trans.setPayType("0001");
      trans.setCompanyCode("");
      trans.setTxnType("04");
      trans.setTxnSubType("");
      trans.setBizType("000201");
      trans.setAccType("");
      trans.setBillType("");
      trans.setBillNo("");
      trans.setInteractMode("");
      trans.setOrigQryId((String)params.get("origQryId"));
      trans.setMerId((String)params.get("merId"));
      trans.setDivideType("");
      trans.setSubMerId("000000000000000");
      trans.setSubMerAbbr("");
      trans.setDivideAmount("000000000000");
      trans.setClearing("C000000051511");
      trans.setTermId("01080209");
      trans.setMerReserved("201511121751461000000000000092");
      trans.setDiscount("0000");
      trans.setInvoice("00");
      trans.setAdditionThirdPartyFee("00");
      trans.setStage("");
      trans.setTransactionMedia("7");
      trans.setOriginalOrderId("");
      trans.setTxnTime(new Date());
      trans.setCreatedAt(new Date());
      trans.setUpdatedAt(new Date());
      this.payWriteService.createMockUnionpayTrans(trans);
   }
}
