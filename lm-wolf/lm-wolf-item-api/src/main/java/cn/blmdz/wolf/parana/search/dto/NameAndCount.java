package cn.blmdz.wolf.parana.search.dto;

import java.beans.ConstructorProperties;
import java.io.Serializable;

public class NameAndCount implements Serializable {
   private static final long serialVersionUID = -8605220179170921347L;
   private String name;
   private Integer count;

   public String getName() {
      return this.name;
   }

   public Integer getCount() {
      return this.count;
   }

   public void setName(String name) {
      this.name = name;
   }

   public void setCount(Integer count) {
      this.count = count;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof NameAndCount)) {
         return false;
      } else {
         NameAndCount other = (NameAndCount)o;
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
      return other instanceof NameAndCount;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      Object $count = this.getCount();
      result = result * 59 + ($count == null?0:$count.hashCode());
      return result;
   }

   public String toString() {
      return "NameAndCount(name=" + this.getName() + ", count=" + this.getCount() + ")";
   }

   public NameAndCount() {
   }

   @ConstructorProperties({"name", "count"})
   public NameAndCount(String name, Integer count) {
      this.name = name;
      this.count = count;
   }
}
