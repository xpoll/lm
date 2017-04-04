package io.terminus.lib.pay.channel.wechatpay.request;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.MoreObjects;
import com.google.common.base.Preconditions;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.lib.pay.channel.wechatpay.dto.WxPayNotifyDto;
import io.terminus.lib.pay.channel.wechatpay.request.WxRequest;
import io.terminus.lib.pay.channel.wechatpay.request.WxToken;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WxQueryRequest extends WxRequest {
   private static final Logger log = LoggerFactory.getLogger(WxQueryRequest.class);
   private static final String SEARCH_ORDER_URL = "https://api.mch.weixin.qq.com/pay/orderquery";

   protected WxQueryRequest(WxToken config) {
      super(config);
   }

   public static WxQueryRequest build(WxToken config) {
      return new WxQueryRequest(config);
   }

   public WxQueryRequest transactionId(String transactionId) {
      this.params.put("transaction_id", transactionId);
      return this;
   }

   public WxQueryRequest outTradeNo(String outTradeNo) {
      this.params.put("out_trade_no", outTradeNo);
      return this;
   }

   public Response query() {
      Response<WxPayNotifyDto> result = new Response();
      Preconditions.checkArgument(!Arguments.isEmpty(MoreObjects.firstNonNull(this.params.get("out_trade_no"), "").toString()) || !Arguments.isEmpty(MoreObjects.firstNonNull(this.params.get("transaction_id"), "").toString()), "params.atleast.one");
      super.nonceStr();
      super.sign();
      String preXml = toXml(this.params);
      log.debug("wx order query url: {}", "https://api.mch.weixin.qq.com/pay/orderquery");
      log.debug("wx order query xml: {}", preXml);
      String body = HttpRequest.post("https://api.mch.weixin.qq.com/pay/orderquery").send(preXml).trustAllCerts().trustAllHosts().body();
      log.debug("wx order query body: {}", body);
      if(verify(body, this.config)) {
         WxPayNotifyDto wxPayNotifyDto = (WxPayNotifyDto)parse(body, WxPayNotifyDto.class);
         result.setResult(wxPayNotifyDto);
      }

      return result;
   }
}
