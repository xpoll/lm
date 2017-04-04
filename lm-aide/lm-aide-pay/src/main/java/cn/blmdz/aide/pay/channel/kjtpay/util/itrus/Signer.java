package cn.blmdz.aide.pay.channel.kjtpay.util.itrus;

import cn.blmdz.aide.pay.channel.kjtpay.pojo.VerifyResult;

public interface Signer {
   String sign(String oriText, String privateKey, String charset) throws Exception;

   VerifyResult verify(String oriText, String sign, String publicKey, String charset) throws Exception;
}
