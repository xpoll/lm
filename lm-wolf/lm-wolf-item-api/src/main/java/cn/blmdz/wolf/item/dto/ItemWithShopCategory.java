package cn.blmdz.wolf.item.dto;

import java.util.List;

import cn.blmdz.wolf.item.model.Item;

public class ItemWithShopCategory extends Item {
   private static final long serialVersionUID = 0L;
   private List shopCategories;

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ItemWithShopCategory)) {
         return false;
      } else {
         ItemWithShopCategory other = (ItemWithShopCategory)o;
         if(!other.canEqual(this)) {
            return false;
         } else if(!super.equals(o)) {
            return false;
         } else {
            Object this$shopCategories = this.getShopCategories();
            Object other$shopCategories = other.getShopCategories();
            if(this$shopCategories == null) {
               if(other$shopCategories != null) {
                  return false;
               }
            } else if(!this$shopCategories.equals(other$shopCategories)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof ItemWithShopCategory;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      result = result * 59 + super.hashCode();
      Object $shopCategories = this.getShopCategories();
      result = result * 59 + ($shopCategories == null?0:$shopCategories.hashCode());
      return result;
   }

   public List getShopCategories() {
      return this.shopCategories;
   }

   public void setShopCategories(List shopCategories) {
      this.shopCategories = shopCategories;
   }

   public String toString() {
      return "ItemWithShopCategory(shopCategories=" + this.getShopCategories() + ")";
   }
}
