package cn.blmdz.aide.pay.channel.alipay.request;

import cn.blmdz.aide.pay.common.Token;
import lombok.Data;

@Data
public class AlipayToken implements Token {
   private String pid;
   private String key;
   private String account;
   private String gateway;
   private String wapGateway;
}
