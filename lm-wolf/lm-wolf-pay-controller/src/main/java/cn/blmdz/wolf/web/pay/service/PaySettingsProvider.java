package cn.blmdz.wolf.web.pay.service;

import cn.blmdz.aide.pay.channel.alipay.request.AlipayToken;
import cn.blmdz.aide.pay.channel.wechatpay.request.WxToken;
import cn.blmdz.wolf.web.pay.dto.AlipaySettingsDto;
import cn.blmdz.wolf.web.pay.dto.KjtSettingsDto;
import cn.blmdz.wolf.web.pay.dto.UnionSettingsDto;
import cn.blmdz.wolf.web.pay.dto.WechatSettingsDto;

public interface PaySettingsProvider {
   AlipayToken buildAlipayToken();

   WxToken buildWxToken();

   AlipaySettingsDto getAlipaySettings();

   KjtSettingsDto getKjtSettings();

   UnionSettingsDto getUnionSettings();

   WechatSettingsDto getWechatSettings();
}
