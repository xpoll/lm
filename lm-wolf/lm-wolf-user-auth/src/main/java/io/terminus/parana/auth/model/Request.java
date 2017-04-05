package io.terminus.parana.auth.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import java.io.Serializable;

@JsonInclude(Include.NON_NULL)
public class Request implements Comparable, Serializable {
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

   public String getGet() {
      return this.get;
   }

   public String getPut() {
      return this.put;
   }

   public String getPost() {
      return this.post;
   }

   public String getDelete() {
      return this.delete;
   }

   public void setGet(String get) {
      this.get = get;
   }

   public void setPut(String put) {
      this.put = put;
   }

   public void setPost(String post) {
      this.post = post;
   }

   public void setDelete(String delete) {
      this.delete = delete;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Request)) {
         return false;
      } else {
         Request other = (Request)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$get = this.getGet();
            Object other$get = other.getGet();
            if(this$get == null) {
               if(other$get != null) {
                  return false;
               }
            } else if(!this$get.equals(other$get)) {
               return false;
            }

            Object this$put = this.getPut();
            Object other$put = other.getPut();
            if(this$put == null) {
               if(other$put != null) {
                  return false;
               }
            } else if(!this$put.equals(other$put)) {
               return false;
            }

            Object this$post = this.getPost();
            Object other$post = other.getPost();
            if(this$post == null) {
               if(other$post != null) {
                  return false;
               }
            } else if(!this$post.equals(other$post)) {
               return false;
            }

            Object this$delete = this.getDelete();
            Object other$delete = other.getDelete();
            if(this$delete == null) {
               if(other$delete != null) {
                  return false;
               }
            } else if(!this$delete.equals(other$delete)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof Request;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $get = this.getGet();
      result = result * 31 + ($get == null?0:$get.hashCode());
      Object $put = this.getPut();
      result = result * 31 + ($put == null?0:$put.hashCode());
      Object $post = this.getPost();
      result = result * 31 + ($post == null?0:$post.hashCode());
      Object $delete = this.getDelete();
      result = result * 31 + ($delete == null?0:$delete.hashCode());
      return result;
   }

   public String toString() {
      return "Request(get=" + this.getGet() + ", put=" + this.getPut() + ", post=" + this.getPost() + ", delete=" + this.getDelete() + ")";
   }
}
