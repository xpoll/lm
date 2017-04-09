package cn.blmdz.wolf.auth.model;

import java.beans.ConstructorProperties;
import java.io.Serializable;
import java.util.Map;

import lombok.Data;

@Data
public class Req implements Serializable {
   private static final long serialVersionUID = 0L;
   private String path;
   private String method;
   private Map<String, String[]> params;

   @ConstructorProperties({"path", "method", "params"})
   public Req(String path, String method, Map<String, String[]> params) {
      this.path = path;
      this.method = method;
      this.params = params;
   }
}
