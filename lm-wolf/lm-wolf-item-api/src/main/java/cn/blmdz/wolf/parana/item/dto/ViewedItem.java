package cn.blmdz.wolf.parana.item.dto;

import java.io.Serializable;
import java.util.List;

import cn.blmdz.wolf.parana.item.model.Item;

public class ViewedItem implements Serializable {
   private static final long serialVersionUID = -9189153272060787427L;
   private Item item;
   private List groupedSkuAttrs;
   private List skus;
   private List imageInfos;

   public Item getItem() {
      return this.item;
   }

   public List getGroupedSkuAttrs() {
      return this.groupedSkuAttrs;
   }

   public List getSkus() {
      return this.skus;
   }

   public List getImageInfos() {
      return this.imageInfos;
   }

   public void setItem(Item item) {
      this.item = item;
   }

   public void setGroupedSkuAttrs(List groupedSkuAttrs) {
      this.groupedSkuAttrs = groupedSkuAttrs;
   }

   public void setSkus(List skus) {
      this.skus = skus;
   }

   public void setImageInfos(List imageInfos) {
      this.imageInfos = imageInfos;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ViewedItem)) {
         return false;
      } else {
         ViewedItem other = (ViewedItem)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$item = this.getItem();
            Object other$item = other.getItem();
            if(this$item == null) {
               if(other$item != null) {
                  return false;
               }
            } else if(!this$item.equals(other$item)) {
               return false;
            }

            Object this$groupedSkuAttrs = this.getGroupedSkuAttrs();
            Object other$groupedSkuAttrs = other.getGroupedSkuAttrs();
            if(this$groupedSkuAttrs == null) {
               if(other$groupedSkuAttrs != null) {
                  return false;
               }
            } else if(!this$groupedSkuAttrs.equals(other$groupedSkuAttrs)) {
               return false;
            }

            Object this$skus = this.getSkus();
            Object other$skus = other.getSkus();
            if(this$skus == null) {
               if(other$skus != null) {
                  return false;
               }
            } else if(!this$skus.equals(other$skus)) {
               return false;
            }

            Object this$imageInfos = this.getImageInfos();
            Object other$imageInfos = other.getImageInfos();
            if(this$imageInfos == null) {
               if(other$imageInfos != null) {
                  return false;
               }
            } else if(!this$imageInfos.equals(other$imageInfos)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof ViewedItem;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $item = this.getItem();
      result = result * 59 + ($item == null?0:$item.hashCode());
      Object $groupedSkuAttrs = this.getGroupedSkuAttrs();
      result = result * 59 + ($groupedSkuAttrs == null?0:$groupedSkuAttrs.hashCode());
      Object $skus = this.getSkus();
      result = result * 59 + ($skus == null?0:$skus.hashCode());
      Object $imageInfos = this.getImageInfos();
      result = result * 59 + ($imageInfos == null?0:$imageInfos.hashCode());
      return result;
   }

   public String toString() {
      return "ViewedItem(item=" + this.getItem() + ", groupedSkuAttrs=" + this.getGroupedSkuAttrs() + ", skus=" + this.getSkus() + ", imageInfos=" + this.getImageInfos() + ")";
   }
}
