package cn.blmdz.wolf.web.mock.pay.manager;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.net.URLDecoder;
import java.net.URLEncoder;
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

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.google.common.base.Objects;
import com.google.common.base.Optional;
import com.google.common.base.Preconditions;
import com.google.common.base.Splitter;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.hash.Hashing;
import com.thoughtworks.xstream.XStream;

import cn.blmdz.aide.pay.channel.alipay.dto.AlipaySettlementResponse;
import cn.blmdz.aide.pay.channel.alipay.dto.settlement.AlipaySettlementDto;
import cn.blmdz.aide.pay.channel.alipay.dto.settlement.AlipaySettlementPaging;
import cn.blmdz.aide.pay.channel.alipay.dto.settlement.AlipaySettlementParam;
import cn.blmdz.aide.pay.channel.alipay.dto.settlement.AlipaySettlementResult;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.wolf.pay.mock.model.MockedAlipayTrans;
import cn.blmdz.wolf.pay.mock.service.MockedAlipayTransService;
import cn.blmdz.wolf.web.mock.pay.annotations.MockPay;
import cn.blmdz.wolf.web.pay.service.PaySettingsProvider;

@Component
@MockPay(
   channels = {"alipay", "wapalipay", "alipay-.+", "mock-alipay"}
)
public class AlipayMockGatewayManager extends AbstractMockGatewayManager {
   private static final Logger log = LoggerFactory.getLogger(AlipayMockGatewayManager.class);
   private final MockedAlipayTransService mockedAlipayTransService;
   private final PaySettingsProvider paySettingsProvider;
   private DateTimeFormatter DFT = DateTimeFormat.forPattern("yyyy-MM-dd");
   private DateTimeFormatter DFT_TIME = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");
   private DateTimeFormatter DATE = DateTimeFormat.forPattern("yyyyMMddHHmmss");
   private static final int DEFAULT_PAGE_NO = 1;
   private static final int DEFAULT_PAGE_SIZE = 500;
   private static final XStream xstream = new XStream();

   @Autowired
   public AlipayMockGatewayManager(MockedAlipayTransService mockedAlipayTransService, PaySettingsProvider paySettingsProvider) {
      this.mockedAlipayTransService = mockedAlipayTransService;
      this.paySettingsProvider = paySettingsProvider;
   }

   public String serve(HttpServletRequest request, HttpServletResponse response) {
      String service = request.getParameter("service");
      if(!Objects.equal(service, "create_direct_pay_by_user") && !Objects.equal(service, "alipay.wap.auth.authAndExecute")) {
         if("alipay.wap.trade.create.direct".equals(service)) {
            return "forward:/mock/pay";
         } else {
            if(Objects.equal(service, "refund_fastpay_by_platform_nopwd")) {
               try {
                  log.info("[Alipay Server]No password refund");
                  response.getWriter().write(this.refund(request));
               } catch (IOException var7) {
                  log.error("sys error,cause:{}", Throwables.getStackTraceAsString(var7));
               }
            }

            if(Objects.equal(service, "account.page.query")) {
               String xml = this.query(request);

               try {
                  response.getWriter().write(xml);
               } catch (IOException var6) {
                  var6.printStackTrace();
               }

               return null;
            } else {
               return null;
            }
         }
      } else {
         log.info("[Alipay Server]Received pay request, then forward:/mock/pay...");
         return "forward:/mock/pay";
      }
   }

   public Map payConfirm(HttpServletRequest request) {
      try {
         log.info("Mocked alipay context:[{}]", request.getParameterMap());
         String id = request.getParameter("orderId");
         Map<String, String> params = Maps.newTreeMap();
         params.put("is_success", "T");
         params.put("out_trade_no", id);
         params.put("trade_no", this.getRandomNo());
         params.put("trade_status", "TRADE_SUCCESS");
         params.put("notify_id", this.getRandomNo());
         params.put("gmt_payment", this.DFT_TIME.print(DateTime.now()));
         String key = this.paySettingsProvider.getAlipaySettings().getKey();
         String toVerify = Joiner.on('&').withKeyValueSeparator("=").join(params);
         String sign = Hashing.md5().newHasher().putString(toVerify, Charsets.UTF_8).putString(key, Charsets.UTF_8).hash().toString();
         params.put("sign", sign);
         params.put("sign_type", "MD5");
         params.put("gmt_payment", URLEncoder.encode(((String)params.get("gmt_payment")).toString(), "utf-8"));
         this.createMockedAlipayTrans(request, params);
         return params;
      } catch (Exception var7) {
         log.error("alipay failed, cause:{}", Throwables.getStackTraceAsString(var7));
         return Maps.newHashMap();
      }
   }

   public String refund(HttpServletRequest request) {
      log.info("[Alipay Server]Refund context:{}", request.getParameterMap());
      String successxml = "<alipay>\n   <is_success>T</is_success>\n</alipay>";
      String errorxml = "<alipay>\n   <is_success>F</is_success>\n</alipay>";

      try {
         String batchNo = request.getParameter("batch_no");
         String num = request.getParameter("batch_num");
         String detail = request.getParameter("detail_data");
         List<String> trades = Splitter.on("#").splitToList(detail);
         List<String> details = Lists.newArrayListWithCapacity(trades.size());

         for(String trade : trades) {
            List<String> infos = Splitter.on("^").splitToList(trade);
            String tradeNo = (String)infos.get(0);
            String fee = (String)infos.get(1);
            String success = "SUCCESS";
            details.add(Joiner.on("^").join(tradeNo, fee, new Object[]{success}));
            this.createMockedRefundTrans(tradeNo, fee);
         }

         String dd = Joiner.on("#").join(details);
         Map<String, Object> params = Maps.newTreeMap();
         params.put("notify_time", this.DFT.print(DateTime.now()));
         params.put("notify_type", "trade_status_sync");
         params.put("notify_id", "70fec0c2730b27528665af4517c27b95");
         params.put("batch_no", batchNo);
         params.put("success_num", num);
         params.put("result_details", dd);
         String toVerify = Joiner.on('&').withKeyValueSeparator("=").join(params);
         String key = this.paySettingsProvider.getAlipaySettings().getKey();
         String sign = Hashing.md5().newHasher().putString(toVerify, Charsets.UTF_8).putString(key, Charsets.UTF_8).hash().toString();
         params.put("result_details", URLEncoder.encode(dd, "utf-8"));
         params.put("sign", sign);
         params.put("sign_type", "MD5");
         String url = request.getParameter("notify_url");
         String suffix = Joiner.on('&').withKeyValueSeparator("=").join(params);
         String result = HttpRequest.get(url + "?" + suffix).connectTimeout(10000).readTimeout(10000).body();
         log.info("[Alipay Server] Async refund result notify \n\t url:   [{}]\n\t params:[{}]\n\t result:[{}]", new Object[]{url, params, result});
         return request.equals("fail")?errorxml:successxml;
      } catch (UnsupportedEncodingException var17) {
         log.error("raise UnsupportedEncodingException :{}", var17.getMessage());
         return successxml;
      }
   }

   private String query(HttpServletRequest request) {
      AlipaySettlementResponse response = new AlipaySettlementResponse();

      try {
         Optional begin = Optional.of(request.getParameter("gmt_start_time"));
         Optional end = Optional.of(request.getParameter("gmt_end_time"));
         Optional<String> pageNoOp = Optional.fromNullable(request.getParameter("page_no"));
         Optional pageSizeOp = Optional.fromNullable(request.getParameter("page_size"));
         Optional merchantOutOrderNo = Optional.fromNullable(request.getParameter("merchant_out_order_no"));
         Integer pageNo = Integer.valueOf(pageNoOp.isPresent()?Integer.parseInt((String)pageNoOp.get()):1);
         Integer pageSize = Integer.valueOf(pageSizeOp.isPresent()?Integer.parseInt(pageSizeOp.get().toString()):500);
         Preconditions.checkArgument(pageSize.intValue() <= 500, "page.size.too.large");
         DateTime startAt = this.DFT_TIME.parseDateTime(begin.get().toString());
         DateTime endAt = this.DFT_TIME.parseDateTime(end.get().toString());
         Optional tradeNoOp = Optional.fromNullable(request.getParameter("trade_no"));
         MockedAlipayTrans criteria = new MockedAlipayTrans();
         if(tradeNoOp.isPresent()) {
            criteria.setTradeNo(tradeNoOp.get().toString());
         }

         if(merchantOutOrderNo.isPresent()) {
            criteria.setMerchantOutOrderNo(merchantOutOrderNo.get().toString());
         }

         Response<Paging<MockedAlipayTrans>> res = this.mockedAlipayTransService.findBy(criteria, startAt.toDate(), endAt.toDate(), pageNo, pageSize);
         Preconditions.checkState(res.isSuccess(), res.getError());
         Paging<MockedAlipayTrans> paging = (Paging)res.getResult();
         response.setSuccess("T");
         AlipaySettlementPaging aPaging = new AlipaySettlementPaging();
         Boolean hasNextPage = Boolean.valueOf(pageNo.intValue() * pageSize.intValue() < paging.getTotal().intValue());
         if(hasNextPage.booleanValue()) {
            aPaging.setHasNextPage("T");
         }

         aPaging.setPageNo(pageNo.toString());
         aPaging.setPageSize(pageSize.toString());
         List<AlipaySettlementDto> dtos = this.convertToDtos(paging.getData());
         aPaging.setAccountLogList(dtos);
         AlipaySettlementResult aRes = new AlipaySettlementResult();
         aRes.setPaging(aPaging);
         response.setResult(aRes);
         response.setSign("");
         response.setSignType("MD5");
         List<AlipaySettlementParam> params = Lists.newArrayList(new AlipaySettlementParam[]{new AlipaySettlementParam("gmt_start_time", begin.get().toString()), new AlipaySettlementParam("gmt_end_time", end.get().toString())});
         response.setRequest(params);
      } catch (Exception var21) {
         log.error("fail to query trans with request:{}, cause:{}", request.getParameterMap(), Throwables.getStackTraceAsString(var21));
      }

      return xstream.toXML(response);
   }

   private List convertToDtos(List<MockedAlipayTrans> data) {
      List<AlipaySettlementDto> dtos = Lists.newArrayList();

      for(MockedAlipayTrans trans : data) {
         AlipaySettlementDto dto = new AlipaySettlementDto();
         BeanMapper.copy(trans, dto);
         dtos.add(dto);
      }

      return dtos;
   }

   private void createMockedAlipayTrans(HttpServletRequest request, Map params) throws UnsupportedEncodingException {
      MockedAlipayTrans inTrans = new MockedAlipayTrans();
      inTrans.setBalance("1000000.00");
      inTrans.setBuyerAccount("20885029701057420156");
      inTrans.setCurrency("156");
      if(Arguments.notEmpty(request.getParameter("bank"))) {
         inTrans.setDepositBankNo("20140621271234");
         inTrans.setSignProductName("普通网银直联收款");
         inTrans.setBankName(request.getParameter("bank"));
      } else {
         inTrans.setSignProductName("大快捷即时到帐收款");
      }

      inTrans.setIncome(request.getParameter("fee"));
      inTrans.setIwAccountLogId(this.getRandomNo() + "0");
      inTrans.setMemo("");
      inTrans.setOutcome("0.00");
      inTrans.setPartnerId("2088011718922324");
      inTrans.setSellerAccount("20880117189223240156");
      inTrans.setSellerFullname("杭州端点网络科技有限公司");
      inTrans.setServiceFee("0.00");
      inTrans.setServiceFeeRatio("0.0");
      inTrans.setMerchantOutOrderNo(request.getParameter("orderId"));
      inTrans.setTotalFee(request.getParameter("fee"));
      inTrans.setTradeNo((String)params.get("trade_no"));
      inTrans.setTradeRefundAmount("0.00");
      inTrans.setTransDate(URLDecoder.decode((String)params.get("gmt_payment"), "utf-8"));
      inTrans.setSubTransCodeMsg("快速支付,支付给个人，支付宝帐户全额");
      inTrans.setRate("0.01");
      Response<Long> res = this.mockedAlipayTransService.create(inTrans);
      Preconditions.checkState(res.isSuccess(), res.getError());
      MockedAlipayTrans outTrans = new MockedAlipayTrans();
      BeanMapper.copy(inTrans, outTrans);
      outTrans.setIncome("0.00");
      outTrans.setIwAccountLogId(this.getRandomNo() + "1");
      outTrans.setMemo("交易服务费[" + inTrans.getTradeNo() + "]");
      outTrans.setOutcome((new BigDecimal(inTrans.getTotalFee())).multiply(new BigDecimal(inTrans.getRate())).toString());
      outTrans.setTransCodeMsg("收费");
      outTrans.setSubTransCodeMsg("收费");
      res = this.mockedAlipayTransService.create(outTrans);
      log.info("Create alipay trans record:[{}] [{}]", outTrans.getMerchantOutOrderNo(), outTrans.getTotalFee());
      log.debug("Create alipay trans record:[{}]", outTrans);
      Preconditions.checkState(res.isSuccess(), res.getError());
   }

   private void createMockedRefundTrans(String tradeNo, String fee) {
      log.info("[Alipay Server]Create mocked refund trans record for:[{}] rmb:[{}]", tradeNo, fee);
      MockedAlipayTrans refund = new MockedAlipayTrans();
      Response<MockedAlipayTrans> trans = this.mockedAlipayTransService.getByTradeNo(tradeNo);
      Preconditions.checkState(trans.isSuccess(), trans.getError());
      BeanMapper.copy(trans.getResult(), refund);
      refund.setBuyerAccount("");
      refund.setCurrency("");
      refund.setMemo("模拟支付退款");
      refund.setOutcome(fee);
      refund.setSellerAccount("");
      refund.setServiceFeeRatio("");
      refund.setTradeRefundAmount(fee);
      refund.setTransDate(this.DFT_TIME.print(DateTime.now()));
      refund.setSubTransCodeMsg("交易退款");
      refund.setIwAccountLogId(this.getRandomNo() + "2");
      Response<Long> createRes = this.mockedAlipayTransService.create(refund);
      log.debug("[Alipay Server]Create mocked refund trans record:[{}]", refund);
      Preconditions.checkState(createRes.isSuccess(), createRes.getError());
   }

   private String getRandomNo() {
      String prefix = this.DATE.print(DateTime.now());
      String suffix = "0000000" + (new Random()).nextInt(100000);
      suffix = suffix.substring(suffix.length() - 7, suffix.length());
      return prefix + "0" + suffix;
   }

   static {
      xstream.autodetectAnnotations(true);
      xstream.processAnnotations(AlipaySettlementResponse.class);
   }
}
