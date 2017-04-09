package cn.blmdz.wolf.search.dto;

import java.beans.ConstructorProperties;
import java.io.Serializable;

public class IdAndName implements Serializable {
   private static final long serialVersionUID = 3244098598487541364L;
   private Long id;
   private String name;

   public Long getId() {
      return this.id;
   }

   public String getName() {
      return this.name;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public void setName(String name) {
      this.name = name;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof IdAndName)) {
         return false;
      } else {
         IdAndName other = (IdAndName)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$id = this.getId();
            Object other$id = other.getId();
            if(this$id == null) {
               if(other$id != null) {
                  return false;
               }
            } else if(!this$id.equals(other$id)) {
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
      return other instanceof IdAndName;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $id = this.getId();
      result = result * 59 + ($id == null?0:$id.hashCode());
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      return result;
   }

   public String toString() {
      return "IdAndName(id=" + this.getId() + ", name=" + this.getName() + ")";
   }

   public IdAndName() {
   }

   @ConstructorProperties({"id", "name"})
   public IdAndName(Long id, String name) {
      this.id = id;
      this.name = name;
   }
}
