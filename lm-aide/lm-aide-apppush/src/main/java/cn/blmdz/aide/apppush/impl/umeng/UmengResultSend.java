package cn.blmdz.aide.apppush.impl.umeng;

import com.google.common.base.Objects;

import lombok.Data;

import java.io.Serializable;

@Data
public class UmengResultSend implements Serializable {
   private static final long serialVersionUID = 1L;
   private String ret;
   private UmengResultSend.ResultData data;

   public Boolean isOk() {
      return Boolean.valueOf(Objects.equal(this.ret, "SUCCESS"));
   }
   
   @Data
   public static class ResultData implements Serializable {
      private static final long serialVersionUID = 1L;
      private String msg_id;
      private String task_id;
      private String error_code;
      private String thirdparty_id;
   }
}
