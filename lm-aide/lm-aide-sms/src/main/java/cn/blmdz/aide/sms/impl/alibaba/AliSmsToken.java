package cn.blmdz.aide.sms.impl.alibaba;

import java.io.Serializable;

import lombok.Data;

@Data
public class AliSmsToken implements Serializable {
   private static final long serialVersionUID = 1L;
   private String appKey;
   private String appSecret;
   private String version;
   private String smsUrl;
}
