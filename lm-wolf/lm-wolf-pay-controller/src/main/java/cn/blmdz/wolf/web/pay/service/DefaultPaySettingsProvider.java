package cn.blmdz.wolf.web.pay.service;

import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import cn.blmdz.aide.pay.channel.alipay.request.AlipayToken;
import cn.blmdz.aide.pay.channel.wechatpay.request.WxToken;
import cn.blmdz.wolf.common.util.ExUtil;
import cn.blmdz.wolf.web.pay.dto.AlipaySettingsDto;
import cn.blmdz.wolf.web.pay.dto.KjtSettingsDto;
import cn.blmdz.wolf.web.pay.dto.UnionSettingsDto;
import cn.blmdz.wolf.web.pay.dto.WechatSettingsDto;

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
