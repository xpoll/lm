package cn.blmdz.aide.pay.channel.kjtpay.request;

import cn.blmdz.aide.pay.common.Token;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class KjtToken implements Token {
   private String pid;
   private String pfxPath;
   private String keyPassword;
   private String cerPath;
   private String account;
   private String transferAccount;
   private String gateway;
}
