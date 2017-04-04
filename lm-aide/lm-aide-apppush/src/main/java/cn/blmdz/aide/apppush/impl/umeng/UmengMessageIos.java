package cn.blmdz.aide.apppush.impl.umeng;

import java.io.Serializable;
import java.util.Map;

import com.google.common.collect.Maps;

import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper=false)
public class UmengMessageIos extends UmengMessage {
   private static final long serialVersionUID = 1L;
   
   private Map<String, Aps> payload = Maps.newHashMap();

   public UmengMessageIos() {
      this.payload.put("aps", new Aps());
   }

   @Data
   public static class Aps implements Serializable {
      private static final long serialVersionUID = 1L;
      private String alert;
      private String badge;
      private String sound;
      private String contentAvailable;
      private String category;
   }
}
