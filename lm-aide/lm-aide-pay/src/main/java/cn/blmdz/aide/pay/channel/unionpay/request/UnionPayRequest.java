package io.terminus.lib.pay.channel.unionpay.request;

import com.google.common.base.Preconditions;
import io.terminus.common.utils.Arguments;
import io.terminus.lib.pay.channel.unionpay.request.Request;
import io.terminus.lib.pay.channel.unionpay.request.UnionToken;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

public class UnionPayRequest extends Request {
   private UnionPayRequest(UnionToken unionToken) {
      super(unionToken);
      this.params.put("txnType", "01");
      this.params.put("txnSubType", "01");
   }

   public static UnionPayRequest build(UnionToken unionToken) {
      return new UnionPayRequest(unionToken);
   }

   public UnionPayRequest bizType(String bizType) {
      if(Arguments.notNull(bizType)) {
         this.params.put("bizType", bizType);
      }

      return this;
   }

   public UnionPayRequest frontUrl(String frontUrl) {
      if(Arguments.notNull(frontUrl)) {
         this.params.put("frontUrl", frontUrl);
      }

      return this;
   }

   public UnionPayRequest backUrl(String backUrl) {
      if(Arguments.notNull(backUrl)) {
         this.params.put("backUrl", backUrl);
      }

      return this;
   }

   public UnionPayRequest channelType(String channelType) {
      if(Arguments.notEmpty(channelType)) {
         this.params.put("channelType", channelType);
      }

      return this;
   }

   public UnionPayRequest orderId(String orderId) {
      this.params.put("orderId", orderId);
      return this;
   }

   public UnionPayRequest txnAmt(Integer txnAmt) {
      Preconditions.checkArgument(Arguments.notNull(txnAmt), "union.pay.amt.empty");
      this.params.put("txnAmt", String.valueOf(txnAmt));
      return this;
   }

   public UnionPayRequest reqReserved(String reqReserved) {
      try {
         this.params.put("reqReserved", URLEncoder.encode(reqReserved, "utf-8"));
      } catch (UnsupportedEncodingException var3) {
         var3.printStackTrace();
      }

      return this;
   }

   public String pay() {
      return super.url();
   }

   private static String formatFee(Integer fee) {
      return DECIMAL_FORMAT.format((double)fee.intValue() / 100.0D);
   }
}
