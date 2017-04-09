package cn.blmdz.wolf.item.dto;

import java.beans.ConstructorProperties;
import java.io.Serializable;

public class ItemIdWithName implements Serializable {
   private static final long serialVersionUID = -1428580613936185378L;
   private Long itemId;
   private Long itemName;

   public String toString() {
      return "ItemIdWithName(itemId=" + this.getItemId() + ", itemName=" + this.getItemName() + ")";
   }

   public ItemIdWithName() {
   }

   @ConstructorProperties({"itemId", "itemName"})
   public ItemIdWithName(Long itemId, Long itemName) {
      this.itemId = itemId;
      this.itemName = itemName;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ItemIdWithName)) {
         return false;
      } else {
         ItemIdWithName other = (ItemIdWithName)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$itemId = this.getItemId();
            Object other$itemId = other.getItemId();
            if(this$itemId == null) {
               if(other$itemId != null) {
                  return false;
               }
            } else if(!this$itemId.equals(other$itemId)) {
               return false;
            }

            Object this$itemName = this.getItemName();
            Object other$itemName = other.getItemName();
            if(this$itemName == null) {
               if(other$itemName != null) {
                  return false;
               }
            } else if(!this$itemName.equals(other$itemName)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof ItemIdWithName;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $itemId = this.getItemId();
      result = result * 59 + ($itemId == null?0:$itemId.hashCode());
      Object $itemName = this.getItemName();
      result = result * 59 + ($itemName == null?0:$itemName.hashCode());
      return result;
   }

   public Long getItemId() {
      return this.itemId;
   }

   public void setItemId(Long itemId) {
      this.itemId = itemId;
   }

   public Long getItemName() {
      return this.itemName;
   }

   public void setItemName(Long itemName) {
      this.itemName = itemName;
   }
}
