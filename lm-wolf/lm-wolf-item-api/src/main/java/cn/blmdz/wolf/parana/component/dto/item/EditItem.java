package cn.blmdz.wolf.parana.component.dto.item;

import java.util.List;

import cn.blmdz.wolf.parana.item.model.Item;
import cn.blmdz.wolf.parana.item.model.ItemDetail;
import cn.blmdz.wolf.parana.rule.dto.BaseOutput;

public class EditItem extends BaseOutput {
   private static final long serialVersionUID = 8812263263491749636L;
   private Item item;
   private List skus;
   private ItemDetail itemDetail;

   public void setGeneralSku(List generalSkus) {
      this.setSkus(generalSkus);
   }

   public Item getItem() {
      return this.item;
   }

   public List getSkus() {
      return this.skus;
   }

   public ItemDetail getItemDetail() {
      return this.itemDetail;
   }

   public void setItem(Item item) {
      this.item = item;
   }

   public void setSkus(List skus) {
      this.skus = skus;
   }

   public void setItemDetail(ItemDetail itemDetail) {
      this.itemDetail = itemDetail;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof EditItem)) {
         return false;
      } else {
         EditItem other = (EditItem)o;
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

            Object this$skus = this.getSkus();
            Object other$skus = other.getSkus();
            if(this$skus == null) {
               if(other$skus != null) {
                  return false;
               }
            } else if(!this$skus.equals(other$skus)) {
               return false;
            }

            Object this$itemDetail = this.getItemDetail();
            Object other$itemDetail = other.getItemDetail();
            if(this$itemDetail == null) {
               if(other$itemDetail != null) {
                  return false;
               }
            } else if(!this$itemDetail.equals(other$itemDetail)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof EditItem;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $item = this.getItem();
      result = result * 59 + ($item == null?0:$item.hashCode());
      Object $skus = this.getSkus();
      result = result * 59 + ($skus == null?0:$skus.hashCode());
      Object $itemDetail = this.getItemDetail();
      result = result * 59 + ($itemDetail == null?0:$itemDetail.hashCode());
      return result;
   }

   public String toString() {
      return "EditItem(item=" + this.getItem() + ", skus=" + this.getSkus() + ", itemDetail=" + this.getItemDetail() + ")";
   }
}
