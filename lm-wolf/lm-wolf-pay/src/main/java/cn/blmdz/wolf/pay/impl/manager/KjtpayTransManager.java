package cn.blmdz.wolf.pay.impl.manager;

import com.google.common.base.Strings;

import cn.blmdz.wolf.pay.common.ThirdPartyFeeAble;
import cn.blmdz.wolf.pay.dto.ThirdPartyFeeDto;

import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class KjtpayTransManager implements ThirdPartyFeeAble {
   private static final Logger log = LoggerFactory.getLogger(KjtpayTransManager.class);

   public ThirdPartyFeeDto getThirdPartyFee(String channel, String paymentCode) {
      return null;
   }

   public List getThirdPartyFeeRefunds(String paymentCode) {
      return null;
   }

   public String getInnerChannel() {
      return "kjtpay";
   }

   public boolean match(String channel) {
      return !Strings.isNullOrEmpty(channel)?(channel.contains("kjtpay")?Boolean.TRUE.booleanValue():Boolean.FALSE.booleanValue()):Boolean.FALSE.booleanValue();
   }
}
