package cn.blmdz.wolf.search.dto;

import java.beans.ConstructorProperties;
import java.io.Serializable;

public class Chosen implements Serializable {
   private static final long serialVersionUID = -7365423698288976174L;
   private int type;
   private Object key;
   private String name;

   public int getType() {
      return this.type;
   }

   public Object getKey() {
      return this.key;
   }

   public String getName() {
      return this.name;
   }

   public void setType(int type) {
      this.type = type;
   }

   public void setKey(Object key) {
      this.key = key;
   }

   public void setName(String name) {
      this.name = name;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Chosen)) {
         return false;
      } else {
         Chosen other = (Chosen)o;
         if(!other.canEqual(this)) {
            return false;
         } else if(this.getType() != other.getType()) {
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

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof Chosen;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      result = result * 59 + this.getType();
      Object $key = this.getKey();
      result = result * 59 + ($key == null?0:$key.hashCode());
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      return result;
   }

   public String toString() {
      return "Chosen(type=" + this.getType() + ", key=" + this.getKey() + ", name=" + this.getName() + ")";
   }

   public Chosen() {
   }

   @ConstructorProperties({"type", "key", "name"})
   public Chosen(int type, Object key, String name) {
      this.type = type;
      this.key = key;
      this.name = name;
   }
}
