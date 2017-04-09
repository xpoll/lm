package cn.blmdz.wolf.category.dto;

import java.io.Serializable;

public class GroupedCategoryAttribute implements Serializable {
   private static final long serialVersionUID = -7116186367791114999L;
   private String group;
   private Iterable categoryAttributes;

   public String getGroup() {
      return this.group;
   }

   public Iterable getCategoryAttributes() {
      return this.categoryAttributes;
   }

   public void setGroup(String group) {
      this.group = group;
   }

   public void setCategoryAttributes(Iterable categoryAttributes) {
      this.categoryAttributes = categoryAttributes;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof GroupedCategoryAttribute)) {
         return false;
      } else {
         GroupedCategoryAttribute other = (GroupedCategoryAttribute)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$group = this.getGroup();
            Object other$group = other.getGroup();
            if(this$group == null) {
               if(other$group != null) {
                  return false;
               }
            } else if(!this$group.equals(other$group)) {
               return false;
            }

            Object this$categoryAttributes = this.getCategoryAttributes();
            Object other$categoryAttributes = other.getCategoryAttributes();
            if(this$categoryAttributes == null) {
               if(other$categoryAttributes != null) {
                  return false;
               }
            } else if(!this$categoryAttributes.equals(other$categoryAttributes)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof GroupedCategoryAttribute;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $group = this.getGroup();
      result = result * 59 + ($group == null?0:$group.hashCode());
      Object $categoryAttributes = this.getCategoryAttributes();
      result = result * 59 + ($categoryAttributes == null?0:$categoryAttributes.hashCode());
      return result;
   }

   public String toString() {
      return "GroupedCategoryAttribute(group=" + this.getGroup() + ", categoryAttributes=" + this.getCategoryAttributes() + ")";
   }
}
