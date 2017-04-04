package io.terminus.lib.pay.channel.wechatpay.request;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Objects;
import com.google.common.base.Preconditions;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.lib.pay.channel.wechatpay.dto.WxPayBillDto;
import io.terminus.lib.pay.channel.wechatpay.dto.WxPayBillResponse;
import io.terminus.lib.pay.channel.wechatpay.request.WxRequest;
import io.terminus.lib.pay.channel.wechatpay.request.WxToken;
import java.util.Date;
import java.util.List;
import java.util.Map;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WxDownloadBillRequest extends WxRequest {
   private static final Logger log = LoggerFactory.getLogger(WxDownloadBillRequest.class);
   private static final String DOWLOAD_BILL_URL = "https://api.mch.weixin.qq.com/pay/downloadbill";
   private static final DateTimeFormatter DTF_DAY = DateTimeFormat.forPattern("yyyyMMdd");
   private static final Splitter NEW_LINE = Splitter.on("\n");
   private static final Splitter APOSTROPHE = Splitter.on(",`");

   protected WxDownloadBillRequest(WxToken config) {
      super(config);
   }

   public static WxDownloadBillRequest build(WxToken config) {
      return new WxDownloadBillRequest(config);
   }

   public WxDownloadBillRequest billType(WxDownloadBillRequest.BillType billType) {
      Preconditions.checkArgument(Arguments.notNull(billType), "wechatpay.billType.empty");
      this.params.put("bill_type", billType.getValue());
      return this;
   }

   public WxDownloadBillRequest billDate(Date billDate) {
      Preconditions.checkArgument(Arguments.notNull(billDate), "wechatpay.billDate.empty");
      this.params.put("bill_date", (new DateTime(billDate)).toString(DTF_DAY));
      return this;
   }

   public Response query() {
      Response<WxPayBillResponse> result = new Response();
      super.nonceStr();
      super.sign();
      String preXml = toXml(this.params);
      log.debug("wx download bill url: {}", "https://api.mch.weixin.qq.com/pay/downloadbill");
      log.debug("wx download bill xml: {}", preXml);
      String body = HttpRequest.post("https://api.mch.weixin.qq.com/pay/downloadbill").send(preXml).trustAllCerts().trustAllHosts().body();
      log.debug("wx refund query body: {}", body);
      if(body.indexOf("xml") != -1) {
         Map error = fromXML(body);
         result.setError(error.get("return_msg").toString());
         return result;
      } else {
         WxPayBillResponse response = parseBillContent(body, this.params.get("bill_type").toString());
         result.setResult(response);
         return result;
      }
   }

   public String queryParam() {
      super.nonceStr();
      super.sign();
      String preXml = toXml(this.params);
      log.debug("wx download bill url: {}", "https://api.mch.weixin.qq.com/pay/downloadbill");
      log.debug("wx download bill xml: {}", preXml);
      return preXml;
   }

   public static WxPayBillResponse parseBillContent(String body, String type) {
      List<String> rows = NEW_LINE.splitToList(body);
      String totalString = (String)rows.get(rows.size() - 2);
      totalString = totalString.substring(1).replace("\r", "");
      WxPayBillResponse payDownloadBillReply = WxPayBillResponse.newInstance(APOSTROPHE.splitToList(totalString));
      List<WxPayBillDto> billDtos = Lists.newArrayList();

      for(int i = 1; i < rows.size() - 3; ++i) {
         String s = (String)rows.get(i);
         s = s.substring(1);
         s = s.replace("\r", "");
         List<String> cols = APOSTROPHE.splitToList(s);
         if(WxDownloadBillRequest.BillType.SUCCESS.getValue().equals(type)) {
            billDtos.add(WxPayBillDto.success(cols));
         } else if(WxDownloadBillRequest.BillType.ALL.getValue().equals(type)) {
            billDtos.add(WxPayBillDto.all(cols));
         } else if(WxDownloadBillRequest.BillType.REFUND.getValue().equals(type)) {
            billDtos.add(WxPayBillDto.refund(cols));
         } else {
            billDtos.add(WxPayBillDto.success(cols));
         }
      }

      payDownloadBillReply.setWxPayBillDtos(billDtos);
      return payDownloadBillReply;
   }

   public static enum BillType {
      ALL("ALL"),
      SUCCESS("SUCCESS"),
      REFUND("REFUND");

      private String value;

      private BillType(String value) {
         this.value = value;
      }

      public static WxDownloadBillRequest.BillType from(String value) {
         for(WxDownloadBillRequest.BillType tradeType : values()) {
            if(Objects.equal(value, tradeType.value)) {
               return tradeType;
            }
         }

         return null;
      }

      public String getValue() {
         return this.value;
      }
   }
}
