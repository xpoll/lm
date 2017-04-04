package io.terminus.lib.pay.channel.unionpay.request;

import io.terminus.common.utils.Arguments;
import io.terminus.lib.pay.channel.unionpay.request.Request;
import io.terminus.lib.pay.channel.unionpay.request.UnionToken;
import java.util.Map;

public class UnionPayQueryStatusRequest extends Request {
   private UnionPayQueryStatusRequest(UnionToken unionToken) {
      super(unionToken);
      this.params.put("txnType", "00");
      this.params.put("txnSubType", "00");
   }

   public static UnionPayQueryStatusRequest build(UnionToken unionToken) {
      return new UnionPayQueryStatusRequest(unionToken);
   }

   public UnionPayQueryStatusRequest bizType(String bizType) {
      if(Arguments.notNull(bizType)) {
         this.params.put("bizType", bizType);
      }

      return this;
   }

   public UnionPayQueryStatusRequest orderId(String orderId) {
      this.params.put("orderId", orderId);
      return this;
   }

   public Map query() {
      Map<String, String> submitFromData = signData(this.param());
      String url = this.unionToken.getSingleQueryUrl();
      Map<String, String> resmap = submitUrl(submitFromData, url);
      return resmap;
   }
}
