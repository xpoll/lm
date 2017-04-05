package io.terminus.parana.web.pay.service;

import io.terminus.lib.pay.channel.alipay.request.AlipayToken;
import io.terminus.lib.pay.channel.kjtpay.request.KjtToken;
import io.terminus.lib.pay.channel.unionpay.request.UnionToken;
import io.terminus.lib.pay.channel.wechatpay.request.WxToken;
import io.terminus.parana.common.util.ExUtil;
import io.terminus.parana.web.pay.dto.AlipaySettingsDto;
import io.terminus.parana.web.pay.dto.KjtSettingsDto;
import io.terminus.parana.web.pay.dto.UnionSettingsDto;
import io.terminus.parana.web.pay.dto.WechatSettingsDto;
import io.terminus.parana.web.pay.service.PaySettingsProvider;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

public class DefaultPaySettingsProvider implements PaySettingsProvider, ApplicationContextAware {
   private ApplicationContext applicationContext;

   public AlipayToken buildAlipayToken() {
      try {
         return (AlipayToken)this.applicationContext.getBean("alipayToken");
      } catch (Exception var2) {
         throw ExUtil.logJsonEx(var2, "buildAlipayToken", new Object[]{""});
      }
   }

   public WxToken buildWxToken() {
      try {
         return (WxToken)this.applicationContext.getBean("wxToken");
      } catch (Exception var2) {
         throw ExUtil.logJsonEx(var2, "buildWxToken", new Object[]{""});
      }
   }

   public KjtToken buildKjtToken() {
      try {
         return (KjtToken)this.applicationContext.getBean("kjtToken");
      } catch (Exception var2) {
         throw ExUtil.logJsonEx(var2, "buildKjtToken", new Object[]{""});
      }
   }

   public UnionToken buildUnionToken() {
      try {
         return (UnionToken)this.applicationContext.getBean("unionToken");
      } catch (Exception var2) {
         throw ExUtil.logJsonEx(var2, "buildUnionToken", new Object[]{""});
      }
   }

   public AlipaySettingsDto getAlipaySettings() {
      try {
         return (AlipaySettingsDto)this.applicationContext.getBean("alipaySettingsDto");
      } catch (Exception var2) {
         throw ExUtil.logJsonEx(var2, "getAlipaySettings", new Object[]{""});
      }
   }

   public KjtSettingsDto getKjtSettings() {
      try {
         return (KjtSettingsDto)this.applicationContext.getBean("kjtSettingsDto");
      } catch (Exception var2) {
         throw ExUtil.logJsonEx(var2, "getKjtSettings", new Object[]{""});
      }
   }

   public UnionSettingsDto getUnionSettings() {
      try {
         return (UnionSettingsDto)this.applicationContext.getBean("unionSettingsDto");
      } catch (Exception var2) {
         throw ExUtil.logJsonEx(var2, "getUnionSettings", new Object[]{""});
      }
   }

   public WechatSettingsDto getWechatSettings() {
      try {
         return (WechatSettingsDto)this.applicationContext.getBean("wechatSettingsDto");
      } catch (Exception var2) {
         throw ExUtil.logJsonEx(var2, "getWechatSettings", new Object[]{""});
      }
   }

   public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
      this.applicationContext = applicationContext;
   }
}
