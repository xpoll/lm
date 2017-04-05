package io.terminus.parana.item.dto;

import java.beans.ConstructorProperties;
import java.io.Serializable;

public class ImageInfo implements Serializable {
   private static final long serialVersionUID = -1023779755022441354L;
   private String name;
   private String url;

   public String getName() {
      return this.name;
   }

   public String getUrl() {
      return this.url;
   }

   public void setName(String name) {
      this.name = name;
   }

   public void setUrl(String url) {
      this.url = url;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ImageInfo)) {
         return false;
      } else {
         ImageInfo other = (ImageInfo)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$name = this.getName();
            Object other$name = other.getName();
            if(this$name == null) {
               if(other$name != null) {
                  return false;
               }
            } else if(!this$name.equals(other$name)) {
               return false;
            }

            Object this$url = this.getUrl();
            Object other$url = other.getUrl();
            if(this$url == null) {
               if(other$url != null) {
                  return false;
               }
            } else if(!this$url.equals(other$url)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof ImageInfo;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      Object $url = this.getUrl();
      result = result * 59 + ($url == null?0:$url.hashCode());
      return result;
   }

   public String toString() {
      return "ImageInfo(name=" + this.getName() + ", url=" + this.getUrl() + ")";
   }

   public ImageInfo() {
   }

   @ConstructorProperties({"name", "url"})
   public ImageInfo(String name, String url) {
      this.name = name;
      this.url = url;
   }
}
