package io.terminus.lib.pay.channel.wechatpay.request;

import com.google.common.base.Preconditions;
import io.terminus.common.utils.Arguments;
import io.terminus.common.utils.JsonMapper;
import io.terminus.lib.pay.channel.wechatpay.request.WxRequest;
import io.terminus.lib.pay.channel.wechatpay.request.WxToken;
import org.joda.time.DateTime;

public class WxJsApiRequest extends WxRequest {
   protected WxJsApiRequest(WxToken config) {
      super(config);
      this.params.remove("mch_id");
      this.params.remove("appid");
      this.params.put("appId", config.getAppId());
   }

   public static WxJsApiRequest build(WxToken config) {
      return new WxJsApiRequest(config);
   }

   public WxJsApiRequest prepayId(String prepayId) {
      Preconditions.checkArgument(Arguments.notEmpty(prepayId), "wechatpay.prepayId.empty");
      this.params.put("package", "prepay_id=" + prepayId);
      return this;
   }

   public String getJson() {
      this.sign();
      return JsonMapper.nonEmptyMapper().toJson(this.params);
   }

   public void sign() {
      this.params.put("signType", "MD5");
      this.params.put("timeStamp", Long.valueOf(DateTime.now().toDate().getTime()).toString());
      this.nonceStr();
      super.sign();
      String paySign = this.params.get("sign").toString();
      this.params.remove("sign");
      this.params.put("paySign", paySign);
   }

   protected void nonceStr() {
      this.params.put("nonceStr", getRandomString());
   }
}
