package cn.blmdz.wolf.category.dto;

import com.google.common.collect.Lists;

import cn.blmdz.wolf.category.model.FrontCategory;

import java.io.Serializable;
import java.util.List;

public class FrontCategoryTree implements Serializable {
   private static final long serialVersionUID = 3530596758471337445L;
   private FrontCategory current;
   private List<FrontCategoryTree> children = Lists.newArrayList();

   public FrontCategory getCurrent() {
      return this.current;
   }

   public List<FrontCategoryTree> getChildren() {
      return this.children;
   }

   public void setCurrent(FrontCategory current) {
      this.current = current;
   }

   public void setChildren(List children) {
      this.children = children;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof FrontCategoryTree)) {
         return false;
      } else {
         FrontCategoryTree other = (FrontCategoryTree)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$current = this.getCurrent();
            Object other$current = other.getCurrent();
            if(this$current == null) {
               if(other$current != null) {
                  return false;
               }
            } else if(!this$current.equals(other$current)) {
               return false;
            }

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
      return other instanceof FrontCategoryTree;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $current = this.getCurrent();
      result = result * 59 + ($current == null?0:$current.hashCode());
      Object $children = this.getChildren();
      result = result * 59 + ($children == null?0:$children.hashCode());
      return result;
   }

   public String toString() {
      return "FrontCategoryTree(current=" + this.getCurrent() + ", children=" + this.getChildren() + ")";
   }
}
