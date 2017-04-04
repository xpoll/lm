package cn.blmdz.aide.pay.channel.kjtpay.verify;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Strings;

import cn.blmdz.aide.pay.channel.kjtpay.pojo.VerifyResult;
import cn.blmdz.aide.pay.channel.kjtpay.util.CoreTransfer;
import cn.blmdz.aide.pay.channel.kjtpay.util.itrus.ITrus;

public class verifyTransferClient {
   private static final Logger logger = LoggerFactory.getLogger(verifyTransferClient.class);
   private static ITrus itrus = new ITrus();

   public verifyTransferClient(String cvmConfigFile, String keyPasswordTrans, String pfxFileNameTrans) {
      itrus.setCvmConfigFile(cvmConfigFile);
      itrus.setKeyPassword(keyPasswordTrans);
      itrus.setPfxFileName(pfxFileNameTrans);
      itrus.init();
   }

   public static VerifyResult verifyBasic(String charset, Map<String, String> formattedParameters) throws Exception {
      String signContent = CoreTransfer.createLinkString(CoreTransfer.paraFilter(formattedParameters), false);
      String signMsg = (String)formattedParameters.get("sign");
      if(logger.isInfoEnabled()) {
         logger.info("verify signature: { content:" + signContent + ", signMsg:" + signMsg + "}");
      }

      VerifyResult result = verifyParameters(signContent, signMsg, charset);
      if(!result.isSuccess()) {
         logger.error(";request dosen\'t pass verify.");
         throw new Exception("验签未通过");
      } else {
         String identityNo = (String)formattedParameters.get("identity_no");
         if(result.isNeedPostCheck() && !Strings.isNullOrEmpty(identityNo)) {
            Map<String, Object> map = result.getInfo();
            if(map != null && !identityNo.equals(map.get("identityNo"))) {
               logger.error("会员标识与证书持有者不匹配,会员标识：" + identityNo + "，证书持有者：" + map.get("identityNo"));
               throw new Exception("会员标识与证书持有者不匹配");
            }
         }

         if(logger.isDebugEnabled()) {
            logger.debug("invoke verify end:" + result.isSuccess());
         }

         return result;
      }
   }

   public static VerifyResult verifyParameters(String content, String signature, String charset) throws Exception {
      if(signature != null) {
         signature = signature.replace(' ', '+');
      }

      new VerifyResult();

      try {
         VerifyResult result = itrus.verify(content, signature, (String)null, charset);
         return result;
      } catch (Exception var5) {
         logger.error("verify failure for content:" + content + ",signature:" + signature, var5);
         throw new Exception("签名失败");
      }
   }
}
