package io.terminus.lib.pay.channel.unionpay.sdk;

import io.terminus.lib.pay.channel.unionpay.sdk.CertUtil;
import io.terminus.lib.pay.channel.unionpay.sdk.HttpClient;
import io.terminus.lib.pay.channel.unionpay.sdk.LogUtil;
import io.terminus.lib.pay.channel.unionpay.sdk.SecureUtil;
import java.io.UnsupportedEncodingException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;
import java.util.TreeMap;
import java.util.Map.Entry;
import org.apache.commons.lang.StringUtils;

public class SDKUtil {
   protected static char[] letter = new char[]{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'};
   protected static final Random random = new Random();

   public static String send(String url, Map data, String encoding, int connectionTimeout, int readTimeout) {
      HttpClient hc = new HttpClient(url, connectionTimeout, readTimeout);
      String res = "";

      try {
         int status = hc.send(data, encoding);
         if(200 == status) {
            res = hc.getResult();
         }
      } catch (Exception var8) {
         LogUtil.writeErrorLog("通信异常", var8);
      }

      return res;
   }

   public static boolean sign(Map data, String encoding) {
      if(isEmpty(encoding)) {
         encoding = "UTF-8";
      }

      data.put("certId", CertUtil.getSignCertId());
      String stringData = coverMap2String(data);
      LogUtil.writeLog("待签名请求报文串:[" + stringData + "]");
      byte[] byteSign = null;
      String stringSign = null;

      try {
         byte[] signDigest = SecureUtil.sha1X16(stringData, encoding);
         byteSign = SecureUtil.base64Encode(SecureUtil.signBySoft(CertUtil.getSignCertPrivateKey(), signDigest));
         stringSign = new String(byteSign);
         data.put("signature", stringSign);
         return true;
      } catch (Exception var6) {
         LogUtil.writeErrorLog("签名异常", var6);
         return false;
      }
   }

   public static boolean signByCertInfo(Map data, String encoding, String certPath, String certPwd) {
      if(isEmpty(encoding)) {
         encoding = "UTF-8";
      }

      if(!isEmpty(certPath) && !isEmpty(certPwd)) {
         data.put("certId", CertUtil.getCertIdByKeyStoreMap(certPath, certPwd));
         String stringData = coverMap2String(data);
         byte[] byteSign = null;
         String stringSign = null;

         try {
            byte[] signDigest = SecureUtil.sha1X16(stringData, encoding);
            byteSign = SecureUtil.base64Encode(SecureUtil.signBySoft(CertUtil.getSignCertPrivateKeyByStoreMap(certPath, certPwd), signDigest));
            stringSign = new String(byteSign);
            data.put("signature", stringSign);
            return true;
         } catch (Exception var8) {
            LogUtil.writeErrorLog("签名异常", var8);
            return false;
         }
      } else {
         LogUtil.writeLog("Invalid Parameter:CertPath=[" + certPath + "],CertPwd=[" + certPwd + "]");
         return false;
      }
   }

   public static boolean validate(Map resData, String encoding) {
      LogUtil.writeLog("验签处理开始");
      if(isEmpty(encoding)) {
         encoding = "UTF-8";
      }

      String stringSign = (String)resData.get("signature");
      String certId = (String)resData.get("certId");
      LogUtil.writeLog("对返回报文串验签使用的验签公钥序列号：[" + certId + "]");
      String stringData = coverMap2String(resData);
      LogUtil.writeLog("待验签返回报文串：[" + stringData + "]");

      try {
         return SecureUtil.validateSignBySoft(CertUtil.getValidateKey(certId), SecureUtil.base64Decode(stringSign.getBytes(encoding)), SecureUtil.sha1X16(stringData, encoding));
      } catch (UnsupportedEncodingException var6) {
         LogUtil.writeErrorLog(var6.getMessage(), var6);
      } catch (Exception var7) {
         LogUtil.writeErrorLog(var7.getMessage(), var7);
      }

      return false;
   }

   public static String coverMap2String(Map data) {
      TreeMap<String, String> tree = new TreeMap();

      for(Entry<String, String> en : data.entrySet()) {
         if(!"signature".equals(((String)en.getKey()).trim())) {
            tree.put(en.getKey(), en.getValue());
         }
      }

      Iterator var5 = tree.entrySet().iterator();
      StringBuffer sf = new StringBuffer();

      while(var5.hasNext()) {
         Entry<String, String> en = (Entry)var5.next();
         sf.append((String)en.getKey() + "=" + (String)en.getValue() + "&");
      }

      return sf.substring(0, sf.length() - 1);
   }

   public static Map coverResultString2Map(String result) {
      return convertResultStringToMap(result);
   }

   public static Map convertResultStringToMap(String result) {
      Map<String, String> map = null;

      try {
         if(StringUtils.isNotBlank(result)) {
            if(result.startsWith("{") && result.endsWith("}")) {
               System.out.println(result.length());
               result = result.substring(1, result.length() - 1);
            }

            map = parseQString(result);
         }
      } catch (UnsupportedEncodingException var3) {
         var3.printStackTrace();
      }

      return map;
   }

   public static Map parseQString(String str) throws UnsupportedEncodingException {
      Map<String, String> map = new HashMap();
      int len = str.length();
      StringBuilder temp = new StringBuilder();
      String key = null;
      boolean isKey = true;
      boolean isOpen = false;
      char openName = 0;
      if(len > 0) {
         for(int i = 0; i < len; ++i) {
            char curChar = str.charAt(i);
            if(isKey) {
               if(curChar == 61) {
                  key = temp.toString();
                  temp.setLength(0);
                  isKey = false;
               } else {
                  temp.append(curChar);
               }
            } else {
               if(isOpen) {
                  if(curChar == openName) {
                     isOpen = false;
                  }
               } else {
                  if(curChar == 123) {
                     isOpen = true;
                     openName = 125;
                  }

                  if(curChar == 91) {
                     isOpen = true;
                     openName = 93;
                  }
               }

               if(curChar == 38 && !isOpen) {
                  putKeyValueToMap(temp, isKey, key, map);
                  temp.setLength(0);
                  isKey = true;
               } else {
                  temp.append(curChar);
               }
            }
         }

         putKeyValueToMap(temp, isKey, key, map);
      }

      return map;
   }

   private static void putKeyValueToMap(StringBuilder temp, boolean isKey, String key, Map map) throws UnsupportedEncodingException {
      if(isKey) {
         key = temp.toString();
         if(key.length() == 0) {
            throw new RuntimeException("QString format illegal");
         }

         map.put(key, "");
      } else {
         if(key.length() == 0) {
            throw new RuntimeException("QString format illegal");
         }

         map.put(key, temp.toString());
      }

   }

   public static String encryptPin(String card, String pwd, String encoding) {
      return SecureUtil.EncryptPin(pwd, card, encoding, CertUtil.getEncryptCertPublicKey());
   }

   public static String encryptCvn2(String cvn2, String encoding) {
      return SecureUtil.EncryptData(cvn2, encoding, CertUtil.getEncryptCertPublicKey());
   }

   public static String decryptCvn2(String base64cvn2, String encoding) {
      return SecureUtil.DecryptedData(base64cvn2, encoding, CertUtil.getSignCertPrivateKey());
   }

   public static String encryptAvailable(String date, String encoding) {
      return SecureUtil.EncryptData(date, encoding, CertUtil.getEncryptCertPublicKey());
   }

   public static String decryptAvailable(String base64Date, String encoding) {
      return SecureUtil.DecryptedData(base64Date, encoding, CertUtil.getSignCertPrivateKey());
   }

   public static String encryptPan(String pan, String encoding) {
      return SecureUtil.EncryptData(pan, encoding, CertUtil.getEncryptCertPublicKey());
   }

   public static String decryptPan(String base64Pan, String encoding) {
      return SecureUtil.DecryptedData(base64Pan, encoding, CertUtil.getSignCertPrivateKey());
   }

   public static String encryptEpInfo(String encryptedInfo, String encoding) {
      return SecureUtil.EncryptData(encryptedInfo, encoding, CertUtil.getEncryptCertPublicKey());
   }

   public static String decryptEpInfo(String base64EncryptedInfo, String encoding) {
      return SecureUtil.DecryptedData(base64EncryptedInfo, encoding, CertUtil.getSignCertPrivateKey());
   }

   public static String encryptTrack(String trackData, String encoding) {
      return SecureUtil.EncryptData(trackData, encoding, CertUtil.getEncryptTrackCertPublicKey());
   }

   public static String encryptTrack(String trackData, String encoding, String modulus, String exponent) {
      return SecureUtil.EncryptData(trackData, encoding, CertUtil.getEncryptTrackCertPublicKey(modulus, exponent));
   }

   public static boolean isEmpty(String s) {
      return null == s || "".equals(s.trim());
   }

   public static String generateTxnTime() {
      return (new SimpleDateFormat("yyyyMMddHHmmss")).format(new Date());
   }

   public static String generateOrderId() {
      StringBuilder sb = new StringBuilder();
      int len = random.nextInt(18);

      for(int i = 0; i < len; ++i) {
         sb.append(letter[i]);
      }

      return generateTxnTime() + sb.toString();
   }

   public static String createAutoSubmitForm(String url, Map data) {
      StringBuffer sf = new StringBuffer();
      sf.append("<form id = \"sform\" action=\"" + url + "\" method=\"post\">");
      if(null != data && 0 != data.size()) {
         for(Entry<String, String> ey : data.entrySet()) {
            String key = (String)ey.getKey();
            String value = (String)ey.getValue();
            sf.append("<input type=\"hidden\" name=\"" + key + "\" id=\"" + key + "\" value=\"" + value + "\"/>");
         }
      }

      sf.append("</form>");
      sf.append("</body>");
      sf.append("<script type=\"text/javascript\">");
      sf.append("document.getElementById(\"sform\").submit();\n");
      sf.append("</script>");
      return sf.toString();
   }

   public static void main(String[] args) {
      System.out.println(encryptTrack("12", "utf-8"));
   }
}
