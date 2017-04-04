package io.terminus.lib.pay.channel.unionpay.request;

import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.lib.pay.channel.unionpay.request.Request;
import io.terminus.lib.pay.channel.unionpay.request.UnionToken;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Date;
import java.util.Map;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UnionRefundRequest extends Request {
   private static final Logger log = LoggerFactory.getLogger(UnionRefundRequest.class);
   private static final DateTimeFormatter DFT_BATCH = DateTimeFormat.forPattern("yyyyMMddHHmmss");

   private UnionRefundRequest(UnionToken token) {
      super(token);
      this.params.put("txnType", "04");
      this.params.put("txnSubType", "00");
   }

   public static UnionRefundRequest build(UnionToken token) {
      return new UnionRefundRequest(token);
   }

   public UnionRefundRequest bizType(String bizType) {
      if(Arguments.notNull(bizType)) {
         this.params.put("bizType", bizType);
      }

      return this;
   }

   public UnionRefundRequest backUrl(String backUrl) {
      if(Arguments.notNull(backUrl)) {
         this.params.put("backUrl", backUrl);
      }

      return this;
   }

   public UnionRefundRequest channelType(String channelType) {
      if(Arguments.notEmpty(channelType)) {
         this.params.put("channelType", channelType);
      }

      return this;
   }

   public UnionRefundRequest orderId(String orderId) {
      this.params.put("orderId", orderId);
      return this;
   }

   public UnionRefundRequest txnAmt(Integer txnAmt) {
      Preconditions.checkArgument(Arguments.notNull(txnAmt), "union.pay.amt.empty");
      this.params.put("txnAmt", String.valueOf(txnAmt));
      return this;
   }

   public UnionRefundRequest reqReserved(String reqReserved) {
      try {
         this.params.put("reqReserved", URLEncoder.encode(reqReserved, "utf-8"));
      } catch (UnsupportedEncodingException var3) {
         var3.printStackTrace();
      }

      return this;
   }

   public UnionRefundRequest origQryId(String origQryId) {
      this.params.put("origQryId", origQryId);
      return this;
   }

   public Response refund() {
      Response<Boolean> result = new Response();
      Map<String, String> submitFromData = signData(this.param());
      String url = this.unionToken.getBackTransUrl();
      Map<String, String> resmap = submitUrl(submitFromData, url);
      if(((String)resmap.get("respCode")).equals("00")) {
         result.setResult(Boolean.TRUE);
         return result;
      } else {
         if(!((String)resmap.get("respCode")).equals("03") && !((String)resmap.get("respCode")).equals("04") && ((String)resmap.get("respCode")).equals("05")) {
            ;
         }

         log.info("请求报文=[" + submitFromData.toString() + "] <br/><br/>" + "同步应答报文=[" + resmap.toString() + "]");
         result.setResult(Boolean.FALSE);
         return result;
      }
   }

   public String url() {
      this.sign();
      String suffix = Joiner.on('&').withKeyValueSeparator("=").join(this.params);
      return this.unionToken.getBackTransUrl() + "?" + suffix;
   }

   public static String toBatchNo(Date refundAt, Long orderTrackId) {
      Preconditions.checkNotNull(orderTrackId, "order.item.id.null");
      Preconditions.checkNotNull(Boolean.valueOf(refundAt != null), "refund.at.null");
      String prefix = DFT_BATCH.print(new DateTime(refundAt));
      String suffix = Strings.padStart(orderTrackId.toString(), 24, '0');
      return prefix + suffix;
   }

   public static Long fromBatchNo(String batchNo) {
      Preconditions.checkArgument(!Strings.isNullOrEmpty(batchNo), "batch.no.empty");
      Preconditions.checkArgument(batchNo.length() == 32, "batch.no.length.illegal");
      int len = batchNo.length();
      return Long.valueOf(batchNo.substring(len - 24, len));
   }
}
