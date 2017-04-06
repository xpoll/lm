package cn.blmdz.wolf.pay.common;

import java.util.List;

import cn.blmdz.wolf.pay.common.PayAbility;
import cn.blmdz.wolf.pay.dto.ThirdPartyFeeDto;

public interface ThirdPartyFeeAble extends PayAbility {
   ThirdPartyFeeDto getThirdPartyFee(String var1, String var2);

   List getThirdPartyFeeRefunds(String var1);

   String getInnerChannel();
}
