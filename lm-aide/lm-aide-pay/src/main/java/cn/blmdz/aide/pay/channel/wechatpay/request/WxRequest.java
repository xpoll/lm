package io.terminus.lib.pay.channel.wechatpay.request;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Joiner;
import com.google.common.base.Objects;
import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;
import com.google.common.hash.HashFunction;
import com.google.common.hash.Hashing;
import com.google.common.io.BaseEncoding;
import com.thoughtworks.xstream.XStream;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.lib.pay.channel.wechatpay.dto.WxSyncResponse;
import io.terminus.lib.pay.channel.wechatpay.request.WxToken;
import io.terminus.lib.pay.driver.CDATAXppDriver;
import io.terminus.lib.pay.driver.PojoMapConverter;
import java.nio.charset.Charset;
import java.util.Date;
import java.util.Map;
import java.util.TreeMap;
import java.util.UUID;
import java.util.Map.Entry;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WxRequest {
   private static final Logger log = LoggerFactory.getLogger(WxRequest.class);
   private static final ObjectMapper objectMapper = new ObjectMapper();
   private static final HashFunction MD5 = Hashing.md5();
   private static final Charset UTF8 = Charset.forName("UTF-8");
   private static final DateTimeFormatter DTF_SECOND = DateTimeFormat.forPattern("yyyyMMddHHmmss");
   protected Map params = Maps.newTreeMap();
   protected WxToken config;
   private static XStream xStream = new XStream(new CDATAXppDriver());

   protected WxRequest(WxToken config) {
      Preconditions.checkArgument(Arguments.notEmpty(config.getAppId()), "wechatpay.appId.empty");
      Preconditions.checkArgument(Arguments.notEmpty(config.getMchId()), "wechatpay.mchId.empty");
      Preconditions.checkArgument(Arguments.notEmpty(config.getPaternerkey()), "wechatpay.paternerkey.empty");
      this.params.put("appid", config.getAppId());
      this.params.put("mch_id", config.getMchId());
      this.config = config;
   }

   public Map param() {
      return this.params;
   }

   protected Response convertToResponse(String body) {
      Response<Boolean> result = new Response();
      Preconditions.checkState(!Strings.isNullOrEmpty(body), "wechatpay.fail");
      WxSyncResponse wxSyncResponse = (WxSyncResponse)xStream.fromXML(body);
      if(wxSyncResponse.isSuccess()) {
         result.setResult(Boolean.TRUE);
      } else {
         log.error("wx raise fail: {}", wxSyncResponse.getErrCodeDes());
         result.setError(wxSyncResponse.getErrCodeDes());
      }

      return result;
   }

   public void sign() {
      try {
         String toVerify = Joiner.on('&').withKeyValueSeparator("=").join(this.params) + "&key=" + this.config.getPaternerkey();
         String sign = BaseEncoding.base16().encode(MD5.newHasher().putString(toVerify, UTF8).hash().asBytes()).toUpperCase();
         this.params.put("sign", sign);
      } catch (Exception var3) {
         throw new RuntimeException(var3);
      }
   }

   protected void nonceStr() {
      this.params.put("nonce_str", getRandomString());
   }

   public static boolean verify(String data, WxToken config) {
      try {
         Map resp = (Map)xStream.fromXML(data);
         Object signObject = resp.get("sign");
         if(signObject == null) {
            log.error("could not find sign value from wx back data, data {}", data);
            return false;
         } else {
            String sign = signObject.toString();
            resp.remove("sign");
            return verify(resp, sign, config);
         }
      } catch (Exception var5) {
         log.error("verify wx back data fail, data:{}, cause:{}", data, Throwables.getStackTraceAsString(var5));
         return false;
      }
   }

   public static boolean verify(Map params, String sign, WxToken config) {
      String toVerify = Joiner.on('&').withKeyValueSeparator("=").join(removeNullValue(params)) + "&key=" + config.getPaternerkey();
      String expect = BaseEncoding.base16().encode(MD5.newHasher().putString(toVerify, UTF8).hash().asBytes()).toUpperCase();
      boolean isSignMatch = Objects.equal(expect, sign);
      if(!isSignMatch) {
         log.error("wechat pay sign mismatch, expected ({}), actual({}), toVerify is:{}", new Object[]{expect, sign, toVerify});
      } else {
         log.info("wechat pay sign matched, expected ({}), actual({}), toVerify is:{}", new Object[]{expect, sign, toVerify});
      }

      return isSignMatch;
   }

   private static Map removeNullValue(Map params) {
      Map<String, String> map = Maps.newHashMap(params);

      for(Entry<String, String> entry : map.entrySet()) {
         if(Strings.isNullOrEmpty((String)entry.getValue())) {
            params.remove(entry.getKey());
         }
      }

      return params;
   }

   protected static String getRandomString() {
      return UUID.randomUUID().toString().replace("-", "");
   }

   protected static String getCurrTime() {
      return DateTime.now().toString(DTF_SECOND);
   }

   protected static String formatDate(Date date) {
      return (new DateTime(date)).toString(DTF_SECOND);
   }

   public static String toXml(Object t) {
      return xStream.toXML(t);
   }

   public static Map fromXML(String xml) {
      return (Map)xStream.fromXML(xml);
   }

   public static Object parse(String xml, Class klass) {
      XStream xs = new XStream();
      xs.autodetectAnnotations(true);
      xs.ignoreUnknownElements();
      xs.processAnnotations(klass);
      return xs.fromXML(xml);
   }

   static {
      xStream.autodetectAnnotations(true);
      xStream.registerConverter(new PojoMapConverter());
      xStream.alias("xml", TreeMap.class);
   }
}
