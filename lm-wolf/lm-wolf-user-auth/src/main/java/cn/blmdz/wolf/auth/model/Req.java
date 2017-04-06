package cn.blmdz.wolf.auth.model;

import java.beans.ConstructorProperties;
import java.io.Serializable;
import java.util.Map;

public class Req implements Serializable {
   private static final long serialVersionUID = 0L;
   private String path;
   private String method;
   private Map params;

   public String getPath() {
      return this.path;
   }

   public String getMethod() {
      return this.method;
   }

   public Map getParams() {
      return this.params;
   }

   public void setPath(String path) {
      this.path = path;
   }

   public void setMethod(String method) {
      this.method = method;
   }

   public void setParams(Map params) {
      this.params = params;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Req)) {
         return false;
      } else {
         Req other = (Req)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$path = this.getPath();
            Object other$path = other.getPath();
            if(this$path == null) {
               if(other$path != null) {
                  return false;
               }
            } else if(!this$path.equals(other$path)) {
               return false;
            }

            Object this$method = this.getMethod();
            Object other$method = other.getMethod();
            if(this$method == null) {
               if(other$method != null) {
                  return false;
               }
            } else if(!this$method.equals(other$method)) {
               return false;
            }

            Object this$params = this.getParams();
            Object other$params = other.getParams();
            if(this$params == null) {
               if(other$params != null) {
                  return false;
               }
            } else if(!this$params.equals(other$params)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof Req;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $path = this.getPath();
      result = result * 31 + ($path == null?0:$path.hashCode());
      Object $method = this.getMethod();
      result = result * 31 + ($method == null?0:$method.hashCode());
      Object $params = this.getParams();
      result = result * 31 + ($params == null?0:$params.hashCode());
      return result;
   }

   public String toString() {
      return "Req(path=" + this.getPath() + ", method=" + this.getMethod() + ", params=" + this.getParams() + ")";
   }

   public Req() {
   }

   @ConstructorProperties({"path", "method", "params"})
   public Req(String path, String method, Map params) {
      this.path = path;
      this.method = method;
      this.params = params;
   }
}
