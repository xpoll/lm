package cn.blmdz.wolf.auth.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

import lombok.Data;

@Data
@JsonInclude(Include.NON_NULL)
public class Request implements Comparable<Request>, Serializable {
   private static final long serialVersionUID = 0L;
   private String get;
   private String put;
   private String post;
   private String delete;

   public int compareTo(Request o) {
      if(o == null) {
         return 1;
      } else {
         int r = this.compare(this.get, o.get);
         if(r != 0) {
            return r;
         } else {
            r = this.compare(this.put, o.put);
            if(r != 0) {
               return r;
            } else {
               r = this.compare(this.post, o.post);
               return r != 0?r:this.compare(this.delete, o.delete);
            }
         }
      }
   }

   private int compare(String a, String b) {
      return a == null?(b == null?0:-1):(b == null?1:a.compareTo(b));
   }
}
