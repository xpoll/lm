package io.terminus.lib.pay.channel.unionpay.request;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Joiner;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;
import io.terminus.common.utils.JsonMapper;
import io.terminus.lib.pay.channel.unionpay.request.UnionToken;
import io.terminus.lib.pay.channel.unionpay.sdk.HttpClient;
import io.terminus.lib.pay.channel.unionpay.sdk.SDKUtil;
import io.terminus.lib.pay.channel.unionpay.sdk.SecureUtil;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Request {
   private static final Logger log = LoggerFactory.getLogger(Request.class);
   protected static final String VERSION = "5.0.0";
   protected static final String ENCODING = "UTF-8";
   protected static final DecimalFormat DECIMAL_FORMAT = new DecimalFormat("0.##");
   protected Map params = Maps.newTreeMap();
   protected UnionToken unionToken;
   String txnTime = (new SimpleDateFormat("yyyyMMddHHmmss")).format(new Date());

   protected Request(UnionToken unionToken) {
      this.params.put("merId", unionToken.getMerId());
      this.params.put("version", "5.0.0");
      this.params.put("encoding", "UTF-8");
      this.params.put("signMethod", "01");
      this.params.put("accessType", "0");
      this.params.put("currencyCode", "156");
      this.params.put("txnTime", this.txnTime);
      this.unionToken = unionToken;
   }

   public Map param() {
      return this.params;
   }

   public String url() {
      this.sign();
      String suffix = Joiner.on('&').withKeyValueSeparator("=").join(this.params);
      return this.unionToken.getFrontTransUrl() + "?" + suffix;
   }

   public String submitPayRequestForm() {
      Map<String, String> submitFromData = signData(this.params);
      submitFromData.put("action", this.unionToken.getFrontTransUrl());
      String formJson = JsonMapper.nonEmptyMapper().toJson(submitFromData);
      log.info("打印请求form，此为请求报文，为联调排查问题的依据：" + formJson);
      return formJson;
   }

   public void sign() {
      try {
         Map<String, String> param = paraFilter(this.params);
         SDKUtil.sign(param, "UTF-8");
      } catch (Exception var2) {
         throw new RuntimeException(var2);
      }
   }

   public static Map paraFilter(Map param) {
      Map<String, String> result = Maps.newTreeMap();
      if(param != null && param.size() > 0) {
         for(String key : param.keySet()) {
            String value = (String)param.get(key);
            if(value != null && !value.equals("") && !key.equalsIgnoreCase("signature")) {
               result.put(key, value);
            }
         }

         return result;
      } else {
         return result;
      }
   }

   public String request(Map params) {
      try {
         HttpRequest request = HttpRequest.post(this.unionToken.getFrontTransUrl()).form(params);
         if(!request.ok()) {
            throw new Exception("Kjtpay request post failed");
         } else {
            return request.url().toString();
         }
      } catch (Exception var3) {
         log.error("pay request(params={}) failed, error code={}", params, Throwables.getStackTraceAsString(var3));
         throw new RuntimeException(var3);
      }
   }

   public static String createHtml(String action, Map hiddens) {
      StringBuffer sf = new StringBuffer();
      sf.append("<html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/></head><body>");
      sf.append("<form id = \"pay_form\" action=\"" + action + "\" method=\"post\">");
      if(null != hiddens && 0 != hiddens.size()) {
         for(Entry<String, String> ey : hiddens.entrySet()) {
            String key = (String)ey.getKey();
            String value = (String)ey.getValue();
            sf.append("<input type=\"hidden\" name=\"" + key + "\" id=\"" + key + "\" value=\"" + value + "\"/>");
         }
      }

      sf.append("</form>");
      sf.append("</body>");
      sf.append("<script type=\"text/javascript\">");
      sf.append("document.all.pay_form.submit();");
      sf.append("</script>");
      sf.append("</html>");
      return sf.toString();
   }

   public static Map signData(Map contentData) {
      Entry<String, String> obj = null;
      Map<String, String> submitFromData = new HashMap();

      for(obj : contentData.entrySet()) {
         String value = (String)obj.getValue();
         if(StringUtils.isNotBlank(value)) {
            submitFromData.put(obj.getKey(), value.trim());
            log.info((String)obj.getKey() + "-->" + value);
         }
      }

      SDKUtil.sign(submitFromData, "UTF-8");
      return submitFromData;
   }

   public static Map signData(Map contentData, String certPath, String certPwd) {
      Entry<String, String> obj = null;
      Map<String, String> submitFromData = new HashMap();
      System.out.println("打印请求报文域 :");

      for(obj : contentData.entrySet()) {
         String value = (String)obj.getValue();
         if(StringUtils.isNotBlank(value)) {
            submitFromData.put(obj.getKey(), value.trim());
            System.out.println((String)obj.getKey() + "-->" + value);
         }
      }

      SDKUtil.signByCertInfo(submitFromData, "UTF-8", certPath, certPwd);
      return submitFromData;
   }

   public static Map submitUrl(Map submitFromData, String requestUrl) {
      String resultString = "";
      log.info("请求银联地址:" + requestUrl);
      HttpClient hc = new HttpClient(requestUrl, 30000, 30000);

      try {
         int status = hc.send(submitFromData, "UTF-8");
         if(200 == status) {
            resultString = hc.getResult();
         }
      } catch (Exception var5) {
         var5.printStackTrace();
      }

      Map<String, String> resData = new HashMap();
      if(null != resultString && !"".equals(resultString)) {
         resData = SDKUtil.convertResultStringToMap(resultString);
         if(SDKUtil.validate(resData, "UTF-8")) {
            log.info("验证签名成功,可以继续后边的逻辑处理");
         } else {
            log.info("验证签名失败,必须验签签名通过才能继续后边的逻辑...");
         }
      }

      return resData;
   }

   private static String enCodeFileContent(String filePath) {
      String baseFileContent = "";
      File file = new File(filePath);
      if(!file.exists()) {
         try {
            file.createNewFile();
         } catch (IOException var16) {
            var16.printStackTrace();
         }
      }

      InputStream in = null;

      try {
         in = new FileInputStream(file);
         int fl = in.available();
         if(null != in) {
            byte[] s = new byte[fl];
            in.read(s, 0, fl);
            baseFileContent = new String(SecureUtil.base64Encode(SecureUtil.deflater(s)));
         }
      } catch (Exception var15) {
         var15.printStackTrace();
      } finally {
         if(null != in) {
            try {
               in.close();
            } catch (IOException var14) {
               var14.printStackTrace();
            }
         }

      }

      return baseFileContent;
   }

   public static void deCodeFileContent(Map resData, String patch) {
      String fileContent = (String)resData.get("fileContent");
      if(null != fileContent && !"".equals(fileContent)) {
         try {
            byte[] fileArray = SecureUtil.inflater(SecureUtil.base64Decode(fileContent.getBytes("UTF-8")));
            String filePath = null;
            if(SDKUtil.isEmpty((String)resData.get("fileName"))) {
               filePath = patch + (String)resData.get("merId") + "_" + (String)resData.get("batchNo") + "_" + (String)resData.get("txnTime") + ".txt";
            } else {
               filePath = patch + (String)resData.get("fileName");
            }

            File file = new File(filePath);
            if(file.exists()) {
               file.delete();
            }

            file.createNewFile();
            FileOutputStream out = new FileOutputStream(file);
            out.write(fileArray, 0, fileArray.length);
            out.flush();
            out.close();
         } catch (UnsupportedEncodingException var7) {
            var7.printStackTrace();
         } catch (IOException var8) {
            var8.printStackTrace();
         }
      }

   }

   public static Map submitDate(Map contentData, String requestUrl) {
      Map<String, String> submitFromData = signData(contentData);
      return submitUrl(submitFromData, requestUrl);
   }

   public static String getCustomerInfo(Map customerInfoMap, String accNo) {
      StringBuffer sf = new StringBuffer("{");
      Iterator<String> it = customerInfoMap.keySet().iterator();

      while(it.hasNext()) {
         String key = (String)it.next();
         String value = (String)customerInfoMap.get(key);
         if(key.equals("pin")) {
            value = SDKUtil.encryptPin(accNo, value, "UTF-8");
         }

         if(it.hasNext()) {
            sf.append(key + "=" + value + "&");
         } else {
            sf.append(key + "=" + value);
         }
      }

      sf.append("}");
      String customerInfo = sf.toString();
      log.info("组装的customerInfo明文：" + customerInfo);

      try {
         return new String(SecureUtil.base64Encode(sf.toString().getBytes("UTF-8")));
      } catch (UnsupportedEncodingException var6) {
         var6.printStackTrace();
      } catch (IOException var7) {
         var7.printStackTrace();
      }

      return customerInfo;
   }

   public static String getCustomerInfoWithEncrypt(Map customerInfoMap, String accNo) {
      StringBuffer sf = new StringBuffer("{");
      StringBuffer encryptedInfoSb = new StringBuffer("");
      Iterator<String> it = customerInfoMap.keySet().iterator();

      while(it.hasNext()) {
         String key = (String)it.next();
         String value = (String)customerInfoMap.get(key);
         if(!key.equals("phoneNo") && !key.equals("cvn2") && !key.equals("expired")) {
            if(key.equals("pin")) {
               value = SDKUtil.encryptPin(accNo, value, "UTF-8");
            }

            if(it.hasNext()) {
               sf.append(key + "=" + value + "&");
            } else {
               sf.append(key + "=" + value);
            }
         } else {
            encryptedInfoSb.append(key + "=" + value + "&");
         }
      }

      if(!encryptedInfoSb.toString().equals("")) {
         encryptedInfoSb.setLength(encryptedInfoSb.length() - 1);
         log.info("组装的customerInfo encryptedInfo明文：" + encryptedInfoSb.toString());
         if(sf.toString().equals("{")) {
            sf.append("encryptedInfo=");
         } else {
            sf.append("&encryptedInfo=");
         }

         sf.append(SDKUtil.encryptEpInfo(encryptedInfoSb.toString(), "UTF-8"));
      }

      sf.append("}");
      String customerInfo = sf.toString();
      log.info("组装的customerInfo明文：" + customerInfo);

      try {
         return new String(SecureUtil.base64Encode(sf.toString().getBytes("UTF-8")));
      } catch (UnsupportedEncodingException var7) {
         var7.printStackTrace();
      } catch (IOException var8) {
         var8.printStackTrace();
      }

      return customerInfo;
   }

   public static String getCardTransData(Map contentData, String encoding) {
      StringBuffer cardTransDataBuffer = new StringBuffer();
      String ICCardData = "uduiadniodaiooxnnxnnada";
      String ICCardSeqNumber = "123";
      String track2Data = "testtrack2Datauidanidnaidiadiada231";
      String track3Data = "testtrack3Datadadaiiuiduiauiduia312117831";
      String transSendMode = "b";
      StringBuffer track2Buffer = new StringBuffer();
      track2Buffer.append(contentData.get("merId")).append("|").append(contentData.get("orderId")).append("|").append(contentData.get("txnTime")).append("|").append(contentData.get("txnAmt")).append("|").append(track2Data);
      String encryptedTrack2 = SDKUtil.encryptTrack(track2Buffer.toString(), encoding);
      StringBuffer track3Buffer = new StringBuffer();
      track3Buffer.append(contentData.get("merId")).append("|").append(contentData.get("orderId")).append("|").append(contentData.get("txnTime")).append("|").append(contentData.get("txnAmt")).append("|").append(track3Data);
      String encryptedTrack3 = SDKUtil.encryptTrack(track3Buffer.toString(), encoding);
      Map<String, String> cardTransDataMap = new HashMap();
      cardTransDataMap.put("ICCardData", ICCardData);
      cardTransDataMap.put("ICCardSeqNumber", ICCardSeqNumber);
      cardTransDataMap.put("track2Data", encryptedTrack2);
      cardTransDataMap.put("track3Data", encryptedTrack3);
      cardTransDataMap.put("transSendMode", transSendMode);
      return cardTransDataBuffer.append("{").append(SDKUtil.coverMap2String(cardTransDataMap)).append("}").toString();
   }

   public static List parseZMFile(String filePath) {
      int[] lengthArray = new int[]{3, 11, 11, 6, 10, 19, 12, 4, 2, 21, 2, 32, 2, 6, 10, 13, 13, 4, 15, 2, 2, 6, 2, 4, 32, 1, 21, 15, 1, 15, 32, 13, 13, 8, 32, 13, 13, 12, 2, 1, 131};
      return parseFile(filePath, lengthArray);
   }

   public static List parseZMEFile(String filePath) {
      int[] lengthArray = new int[]{3, 11, 11, 6, 10, 19, 12, 4, 2, 21, 2, 32, 2, 6, 10, 13, 13, 4, 15, 2, 2, 6, 2, 4, 32, 1, 21, 15, 1, 15, 32, 13, 13, 8, 32, 13, 13, 12, 2, 1, 131};
      return parseFile(filePath, lengthArray);
   }

   private static List parseFile(String filePath, int[] lengthArray) {
      List<Map<Integer, String>> ZmDataList = new ArrayList();

      try {
         String encoding = "UTF-8";
         File file = new File(filePath);
         if(file.isFile() && file.exists()) {
            InputStreamReader read = new InputStreamReader(new FileInputStream(file), encoding);
            BufferedReader bufferedReader = new BufferedReader(read);
            String lineTxt = null;

            while((lineTxt = bufferedReader.readLine()) != null) {
               Map<Integer, String> ZmDataMap = new LinkedHashMap();
               int leftIndex = 0;
               int rightIndex = 0;

               for(int i = 0; i < lengthArray.length; ++i) {
                  rightIndex = leftIndex + lengthArray[i];
                  String filed = lineTxt.substring(leftIndex, rightIndex);
                  leftIndex = rightIndex + 1;
                  ZmDataMap.put(Integer.valueOf(i), filed);
               }

               ZmDataList.add(ZmDataMap);
            }

            read.close();
         } else {
            log.error("找不到指定的文件");
         }
      } catch (Exception var13) {
         log.error("读取文件内容出错 cause:{}", Throwables.getStackTraceAsString(var13));
      }

      for(int i = 0; i < ((List)ZmDataList).size(); ++i) {
         Map<Integer, String> ZmDataMapTmp = (Map)ZmDataList.get(i);

         for(Integer key : ZmDataMapTmp.keySet()) {
            String var19 = (String)ZmDataMapTmp.get(key);
         }
      }

      return ZmDataList;
   }
}
