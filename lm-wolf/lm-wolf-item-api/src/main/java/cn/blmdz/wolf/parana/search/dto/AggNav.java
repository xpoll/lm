package cn.blmdz.wolf.parana.search.dto;

import java.beans.ConstructorProperties;
import java.io.Serializable;

public class AggNav implements Serializable {
   private static final long serialVersionUID = 8091726228902107984L;
   private Object key;
   private String name;
   private Long count;

   public Object getKey() {
      return this.key;
   }

   public String getName() {
      return this.name;
   }

   public Long getCount() {
      return this.count;
   }

   public void setKey(Object key) {
      this.key = key;
   }

   public void setName(String name) {
      this.name = name;
   }

   public void setCount(Long count) {
      this.count = count;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof AggNav)) {
         return false;
      } else {
         AggNav other = (AggNav)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$key = this.getKey();
            Object other$key = other.getKey();
            if(this$key == null) {
               if(other$key != null) {
                  return false;
               }
            } else if(!this$key.equals(other$key)) {
               return false;
            }

            Object this$name = this.getName();
            Object other$name = other.getName();
            if(this$name == null) {
               if(other$name != null) {
                  return false;
               }
            } else if(!this$name.equals(other$name)) {
               return false;
            }

            Object this$count = this.getCount();
            Object other$count = other.getCount();
            if(this$count == null) {
               if(other$count != null) {
                  return false;
               }
            } else if(!this$count.equals(other$count)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof AggNav;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $key = this.getKey();
      result = result * 59 + ($key == null?0:$key.hashCode());
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      Object $count = this.getCount();
      result = result * 59 + ($count == null?0:$count.hashCode());
      return result;
   }

   public String toString() {
      return "AggNav(key=" + this.getKey() + ", name=" + this.getName() + ", count=" + this.getCount() + ")";
   }

   public AggNav() {
   }

   @ConstructorProperties({"key", "name", "count"})
   public AggNav(Object key, String name, Long count) {
      this.key = key;
      this.name = name;
      this.count = count;
   }
}
