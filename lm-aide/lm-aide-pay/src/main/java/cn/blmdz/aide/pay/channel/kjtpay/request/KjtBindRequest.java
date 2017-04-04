package cn.blmdz.aide.pay.channel.kjtpay.request;

import com.google.common.base.Preconditions;

import cn.blmdz.home.common.util.Arguments;

public class KjtBindRequest extends Request {
   private KjtBindRequest(KjtToken kjtToken) {
      super(kjtToken);
      this.params.put("service", "account_bind");
      this.params.put("memo", "");
   }

   public static KjtBindRequest build(KjtToken kjtToken) {
      return new KjtBindRequest(kjtToken);
   }

   public KjtBindRequest returnUrl(String forward) {
      if(Arguments.notNull(forward)) {
         this.params.put("return_url", forward);
      }

      return this;
   }

   public KjtBindRequest notify(String notify) {
      if(Arguments.notNull(notify)) {
         this.params.put("notify_url", notify);
      }

      return this;
   }

   public KjtBindRequest mobile(String mobile) {
      if(Arguments.notEmpty(mobile)) {
         this.params.put("mobile", mobile);
      }

      return this;
   }

   public KjtBindRequest thirdAccount(String thirdAccount) {
      Preconditions.checkArgument(Arguments.notEmpty(thirdAccount), "kjt.pay.third.account.no.empty");
      this.params.put("third_account", thirdAccount);
      return this;
   }

   public KjtBindRequest isReal(String isReal) {
      Preconditions.checkArgument(Arguments.notEmpty(isReal), "kjt.pay.isReal.empty");
      this.params.put("is_real", isReal);
      return this;
   }

   public KjtBindRequest setSign(String sign) {
      Preconditions.checkArgument(Arguments.notNull(sign), "kjt.pay.sign.empty");
      this.params.put("sign", sign);
      return this;
   }

   public KjtBindRequest setSignType(String type) {
      Preconditions.checkArgument(Arguments.notNull(type), "kjt.pay.sign_type.empty");
      this.params.put("sign_type", type);
      return this;
   }

   public String pay() {
      return super.url();
   }
}
