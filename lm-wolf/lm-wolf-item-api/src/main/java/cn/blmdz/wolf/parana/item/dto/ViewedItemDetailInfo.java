package cn.blmdz.wolf.parana.item.dto;

import java.io.Serializable;
import java.util.List;

import cn.blmdz.wolf.parana.item.model.ItemDetail;

public class ViewedItemDetailInfo implements Serializable {
   private static final long serialVersionUID = 899873581042411121L;
   private ItemDetail itemDetail;
   private List groupedOtherAttributes;

   public ItemDetail getItemDetail() {
      return this.itemDetail;
   }

   public List getGroupedOtherAttributes() {
      return this.groupedOtherAttributes;
   }

   public void setItemDetail(ItemDetail itemDetail) {
      this.itemDetail = itemDetail;
   }

   public void setGroupedOtherAttributes(List groupedOtherAttributes) {
      this.groupedOtherAttributes = groupedOtherAttributes;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ViewedItemDetailInfo)) {
         return false;
      } else {
         ViewedItemDetailInfo other = (ViewedItemDetailInfo)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$itemDetail = this.getItemDetail();
            Object other$itemDetail = other.getItemDetail();
            if(this$itemDetail == null) {
               if(other$itemDetail != null) {
                  return false;
               }
            } else if(!this$itemDetail.equals(other$itemDetail)) {
               return false;
            }

            Object this$groupedOtherAttributes = this.getGroupedOtherAttributes();
            Object other$groupedOtherAttributes = other.getGroupedOtherAttributes();
            if(this$groupedOtherAttributes == null) {
               if(other$groupedOtherAttributes != null) {
                  return false;
               }
            } else if(!this$groupedOtherAttributes.equals(other$groupedOtherAttributes)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof ViewedItemDetailInfo;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $itemDetail = this.getItemDetail();
      result = result * 59 + ($itemDetail == null?0:$itemDetail.hashCode());
      Object $groupedOtherAttributes = this.getGroupedOtherAttributes();
      result = result * 59 + ($groupedOtherAttributes == null?0:$groupedOtherAttributes.hashCode());
      return result;
   }

   public String toString() {
      return "ViewedItemDetailInfo(itemDetail=" + this.getItemDetail() + ", groupedOtherAttributes=" + this.getGroupedOtherAttributes() + ")";
   }
}
