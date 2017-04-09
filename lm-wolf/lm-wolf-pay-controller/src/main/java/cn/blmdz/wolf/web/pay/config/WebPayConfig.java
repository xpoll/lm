package cn.blmdz.wolf.web.pay.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.aide.pay.channel.alipay.request.AlipayToken;
import cn.blmdz.aide.pay.channel.wechatpay.request.WxToken;
import cn.blmdz.wolf.web.pay.dto.AlipaySettingsDto;
import cn.blmdz.wolf.web.pay.dto.KjtSettingsDto;
import cn.blmdz.wolf.web.pay.dto.UnionSettingsDto;
import cn.blmdz.wolf.web.pay.dto.WechatSettingsDto;
import cn.blmdz.wolf.web.pay.service.DefaultPaySettingsProvider;
import cn.blmdz.wolf.web.pay.service.PaySettingsProvider;

@Configuration
@ComponentScan({"cn.blmdz.wolf.web.pay.controller", "cn.blmdz.wolf.web.pay.service"})
public class WebPayConfig {
   @Bean
   @ConditionalOnMissingBean
   public PaySettingsProvider payTokenBuilder() {
      return new DefaultPaySettingsProvider();
   }

   @Configuration
   @ConditionalOnProperty(
      name = {"pay.debug"},
      havingValue = "true"
   )
   @ComponentScan({"cn.blmdz.wolf.web.mock.pay"})
   public static class MockPayConfig {
   }

   @Configuration
   public static class WebPayTokenConfig {
      @Bean
      @ConditionalOnMissingBean
      public AlipayToken alipayToken(@Value("${alipay.pid:pid}") String pid, @Value("${alipay.key:key}") String key, @Value("${alipay.account:account}") String account, @Value("${alipay.gateway:gateway}") String gateway, @Value("${alipay.wap.gateway:wapgateway}") String wapGateway) {
         AlipayToken token = new AlipayToken();
         token.setAccount(account);
         token.setGateway(gateway);
         token.setKey(key);
         token.setWapGateway(wapGateway);
         token.setPid(pid);
         return token;
      }

      @Bean
      @ConditionalOnMissingBean
      public AlipaySettingsDto alipaySettingsDto(@Value("${alipay.notify.url:notifyUrl}") String notifyUrl, @Value("${alipay.return.url:returnUrl}") String returnUrl, @Value("${alipay.refund.notify.url:refundNotifyUrl}") String refundNotifyUrl, @Value("${alipay.pid:pid}") String pid, @Value("${alipay.key:key}") String key, @Value("${alipay.account:account}") String account, @Value("${alipay.gateway:gateway}") String gateway, @Value("${alipay.wap.gateway:wapgateway}") String wapGateway) {
         AlipaySettingsDto alipaySettingsDto = new AlipaySettingsDto();
         alipaySettingsDto.setNotifyUrl(notifyUrl);
         alipaySettingsDto.setReturnUrl(returnUrl);
         alipaySettingsDto.setRefundNotifyUrl(refundNotifyUrl);
         alipaySettingsDto.setPid(pid);
         alipaySettingsDto.setKey(key);
         alipaySettingsDto.setAccount(account);
         alipaySettingsDto.setGateway(gateway);
         alipaySettingsDto.setWapGateway(wapGateway);
         return alipaySettingsDto;
      }

      @Bean
      @ConditionalOnMissingBean
      public WxToken wxToken(@Value("${wechat.appid:appid}") String appId, @Value("${wechat.secret:secret}") String secret, @Value("${wechat.mchid:mchid}") String mchId, @Value("${wechat.partner.key:partnerKey}") String paternerkey, @Value("${wechat.gateway:gateway}") String gateway, @Value("${wechat.refund.gateway:refundGateway}") String refundGateway, @Value("${wechat.download.bill.url:dowloadBillUrl}") String dowloadBillUrl, @Value("${wechat.query.refund.gateway:queryRefundGateway}") String queryRefundGateway) {
         WxToken wxToken = new WxToken();
         wxToken.setAppId(appId);
         wxToken.setSecret(secret);
         wxToken.setGateway(gateway);
         wxToken.setDowloadBillUrl(dowloadBillUrl);
         wxToken.setMchId(mchId);
         wxToken.setPaternerkey(paternerkey);
         wxToken.setQueryRefundGateway(queryRefundGateway);
         wxToken.setRefundGateway(refundGateway);
         return wxToken;
      }

      @Bean
      @ConditionalOnMissingBean
      public WechatSettingsDto wechatSettingsDto(@Value("${wechat.caFilePath:caFilePath}") String caFilePath, @Value("${wechat.certFilePath:certFilePath}") String certFilePath, @Value("${wechat.notifyUrl:notifyUrl}") String notifyUrl, @Value("${wechat.returnUrl:returnUrl}") String returnUrl, @Value("${wechat.imageBaseUrl:imageBaseUrl}") String imageBaseUrl, @Value("${wechat.token:token}") String token, @Value("${wechat.publicName:publicName}") String publicName) {
         WechatSettingsDto wechatSettingsDto = new WechatSettingsDto();
         wechatSettingsDto.setCaFilePath(caFilePath);
         wechatSettingsDto.setCertFilePath(certFilePath);
         wechatSettingsDto.setImageBaseUrl(imageBaseUrl);
         wechatSettingsDto.setNotifyUrl(notifyUrl);
         wechatSettingsDto.setReturnUrl(returnUrl);
         wechatSettingsDto.setToken(token);
         wechatSettingsDto.setPublicName(publicName);
         return wechatSettingsDto;
      }

      @Bean
      @ConditionalOnMissingBean
      public UnionSettingsDto unionSettingsDto(@Value("${unionpay.merId:merId}") String merid, @Value("${unionpay.refundNotifyUrl:refundNotifyUrl}") String refundNotifyUrl, @Value("${unionpay.notifyUrl:notifyUrl}") String notifyUrl, @Value("${unionpay.returnUrl:returnUrl}") String returnUrl, @Value("${unionpay.certPath:certPath}") String certPath, @Value("${unionpay.certPwd:certPwd}") String certPwd, @Value("${unionpay.certType:certType}") String certType, @Value("${unionpay.ValidateCertDir:validateCertDir}") String validateCertDir, @Value("${unionpay.encryptCertPath:encryptCertPath}") String encryptCertPath, @Value("${unionpay.singleMode:true}") String singleMode, @Value("${unionpay.transFilePatch:transFilePatch}") String transFilePatch) {
         UnionSettingsDto unionSettingsDto = new UnionSettingsDto();
         unionSettingsDto.setMerid(merid);
         unionSettingsDto.setRefundNotifyUrl(refundNotifyUrl);
         unionSettingsDto.setNotifyUrl(notifyUrl);
         unionSettingsDto.setReturnUrl(returnUrl);
         unionSettingsDto.setCertPath(certPath);
         unionSettingsDto.setCertPwd(certPwd);
         unionSettingsDto.setCertType(certType);
         unionSettingsDto.setValidateCertDir(validateCertDir);
         unionSettingsDto.setEncryptCertPath(encryptCertPath);
         unionSettingsDto.setSingleMode(singleMode);
         unionSettingsDto.setTransFilePatch(transFilePatch);
         return unionSettingsDto;
      }

      @Bean
      @ConditionalOnMissingBean
      public KjtSettingsDto kjtSettingsDto(@Value("${kjtpay.notifyUrl:notifyUrl}") String notifyUrl, @Value("${kjtpay.returnUrl:returnUrl}") String returnUrl, @Value("${kjtpay.refundNotifyUrl:refundNotifyUrl}") String refundNotifyUrl) {
         KjtSettingsDto dto = new KjtSettingsDto();
         dto.setReturnUrl(returnUrl);
         dto.setNotifyUrl(notifyUrl);
         dto.setRefundNotifyUrl(refundNotifyUrl);
         return dto;
      }
   }
}
