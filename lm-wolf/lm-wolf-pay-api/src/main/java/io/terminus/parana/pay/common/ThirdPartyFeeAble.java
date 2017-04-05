package io.terminus.parana.pay.common;

import io.terminus.parana.pay.common.PayAbility;
import io.terminus.parana.pay.dto.ThirdPartyFeeDto;
import java.util.List;

public interface ThirdPartyFeeAble extends PayAbility {
   ThirdPartyFeeDto getThirdPartyFee(String var1, String var2);

   List getThirdPartyFeeRefunds(String var1);

   String getInnerChannel();
}
