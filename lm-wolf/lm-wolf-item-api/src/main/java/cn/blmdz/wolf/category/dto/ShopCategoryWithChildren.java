package cn.blmdz.wolf.category.dto;

import java.util.List;

import cn.blmdz.wolf.category.model.ShopCategory;

public class ShopCategoryWithChildren extends ShopCategory {
   private static final long serialVersionUID = 0L;
   private List children;

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ShopCategoryWithChildren)) {
         return false;
      } else {
         ShopCategoryWithChildren other = (ShopCategoryWithChildren)o;
         if(!other.canEqual(this)) {
            return false;
         } else if(!super.equals(o)) {
            return false;
         } else {
            Object this$children = this.getChildren();
            Object other$children = other.getChildren();
            if(this$children == null) {
               if(other$children != null) {
                  return false;
               }
            } else if(!this$children.equals(other$children)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof ShopCategoryWithChildren;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      result = result * 59 + super.hashCode();
      Object $children = this.getChildren();
      result = result * 59 + ($children == null?0:$children.hashCode());
      return result;
   }

   public List getChildren() {
      return this.children;
   }

   public void setChildren(List children) {
      this.children = children;
   }

   public String toString() {
      return "ShopCategoryWithChildren(children=" + this.getChildren() + ")";
   }
}
