package cn.blmdz.wolf.parana.attribute.dto;

import java.io.Serializable;

public class AttributeKey implements Serializable {
   private static final long serialVersionUID = -261128922692788999L;
   private Long categoryId;
   private String name;
   private String group;

   public String toString() {
      return "AttributeKey(categoryId=" + this.getCategoryId() + ", name=" + this.getName() + ", group=" + this.getGroup() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof AttributeKey)) {
         return false;
      } else {
         AttributeKey other = (AttributeKey)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$categoryId = this.getCategoryId();
            Object other$categoryId = other.getCategoryId();
            if(this$categoryId == null) {
               if(other$categoryId != null) {
                  return false;
               }
            } else if(!this$categoryId.equals(other$categoryId)) {
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
      return other instanceof AttributeKey;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $categoryId = this.getCategoryId();
      result = result * 59 + ($categoryId == null?0:$categoryId.hashCode());
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      return result;
   }

   public Long getCategoryId() {
      return this.categoryId;
   }

   public void setCategoryId(Long categoryId) {
      this.categoryId = categoryId;
   }

   public String getName() {
      return this.name;
   }

   public void setName(String name) {
      this.name = name;
   }

   public String getGroup() {
      return this.group;
   }

   public void setGroup(String group) {
      this.group = group;
   }
}
