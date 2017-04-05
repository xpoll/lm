package io.terminus.parana.web.pay.service;

import io.terminus.lib.pay.channel.alipay.request.AlipayToken;
import io.terminus.lib.pay.channel.kjtpay.request.KjtToken;
import io.terminus.lib.pay.channel.unionpay.request.UnionToken;
import io.terminus.lib.pay.channel.wechatpay.request.WxToken;
import io.terminus.parana.web.pay.dto.AlipaySettingsDto;
import io.terminus.parana.web.pay.dto.KjtSettingsDto;
import io.terminus.parana.web.pay.dto.UnionSettingsDto;
import io.terminus.parana.web.pay.dto.WechatSettingsDto;

public interface PaySettingsProvider {
   AlipayToken buildAlipayToken();

   WxToken buildWxToken();

   KjtToken buildKjtToken();

   UnionToken buildUnionToken();

   AlipaySettingsDto getAlipaySettings();

   KjtSettingsDto getKjtSettings();

   UnionSettingsDto getUnionSettings();

   WechatSettingsDto getWechatSettings();
}
