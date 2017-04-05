package io.terminus.parana.web.mock.pay.controller;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Joiner;
import io.terminus.lib.pay.utils.XmlUtils;
import io.terminus.parana.pay.mock.cacher.WapalipayCache;
import io.terminus.parana.web.mock.pay.gateway.MockGateway;
import io.terminus.parana.web.pay.service.PaySettingsProvider;
import java.io.UnsupportedEncodingException;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping({"/api/mock"})
public class MockPayGatewayControl {
   private static final Logger log = LoggerFactory.getLogger(MockPayGatewayControl.class);
   private final MockGateway mockGateway;
   private final PaySettingsProvider paySettingsProvider;

   @Autowired
   public MockPayGatewayControl(MockGateway mockGateway, PaySettingsProvider paySettingsProvider) {
      this.mockGateway = mockGateway;
      this.paySettingsProvider = paySettingsProvider;
   }

   @RequestMapping({"/{channel}/**"})
   public String gateway(@PathVariable("channel") String channel, HttpServletRequest request, HttpServletResponse response) {
      return this.mockGateway.serve(request, response, channel);
   }

   @RequestMapping(
      value = {"/{channel}/pay"},
      method = {RequestMethod.POST}
   )
   public String payConfirm(@PathVariable("channel") String channel, HttpServletRequest request) throws UnsupportedEncodingException {
      String notifyUrl = request.getParameter("notify");
      String returnUrl = request.getParameter("return");
      Map<String, String> params = this.mockGateway.payConfirm(request, channel);
      String result = "fail";
      if(channel.contains("alipay")) {
         String suffix = Joiner.on('&').withKeyValueSeparator("=").join(params);
         result = HttpRequest.get(notifyUrl + "?" + suffix).connectTimeout(1000000).readTimeout(1000000).body();
         log.info("[Alipay Server] Async pay result notify \n\t url:   [{}]\n\t params:[{}]\n\t result:[{}]", new Object[]{notifyUrl, params, result});
      }

      if(channel.contains("wechatpay")) {
         notifyUrl = this.paySettingsProvider.getWechatSettings().getNotifyUrl();
         returnUrl = this.paySettingsProvider.getWechatSettings().getReturnUrl();
         String sendXml = (String)params.get("sendXml");
         result = HttpRequest.post(notifyUrl).contentType("text/plain", "utf-8").send(sendXml).connectTimeout(1000000).readTimeout(1000000).body();
         log.info("[Wechat pay Server] Async pay result notify \n\t url:   [{}]\n\t result:[{}]", notifyUrl, result);
         result = XmlUtils.fromXML(result).get("return_code").toString();
      }

      if(channel.contains("kjtpay")) {
         notifyUrl = this.paySettingsProvider.getKjtSettings().getNotifyUrl();
         String suffix = Joiner.on('&').withKeyValueSeparator("=").join(params);
         result = HttpRequest.get(notifyUrl + "?" + suffix).connectTimeout(1000000).readTimeout(1000000).body();
         log.info("[Kjtpay Server] Async pay result notify \n\t url:   [{}]\n\t params:[{}]\n\t result:[{}]", new Object[]{notifyUrl, params, result});
      }

      if(channel.contains("unionpay")) {
         String certId = "87542682951161536912959476560149378562";
         params.put("certId", certId);
         String suffix = Joiner.on('&').withKeyValueSeparator("=").join(params);
         result = HttpRequest.get(notifyUrl + "?" + suffix).connectTimeout(1000000).readTimeout(1000000).body();
         log.info("[Kjtpay Server] Async pay result notify \n\t url:   [{}]\n\t params:[{}]\n\t result:[{}]", new Object[]{notifyUrl, params, result});
      }

      return !result.toLowerCase().equals("success") && !result.toLowerCase().equals("ok")?"fail":"redirect:/mock/success?url=" + returnUrl;
   }

   @RequestMapping(
      value = {"/wapalipay/context"},
      produces = {"application/json"}
   )
   @ResponseBody
   public Map getWapalipayContext(@RequestParam("tokenString") String tokenString) {
      return (Map)WapalipayCache.wapalipayCache.getIfPresent(tokenString);
   }
}
