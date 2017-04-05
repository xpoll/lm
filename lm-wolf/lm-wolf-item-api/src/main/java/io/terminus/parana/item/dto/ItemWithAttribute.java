package io.terminus.parana.item.dto;

import io.terminus.parana.item.model.Item;
import io.terminus.parana.item.model.ItemAttribute;
import java.io.Serializable;

public class ItemWithAttribute implements Serializable {
   private static final long serialVersionUID = -4744414284615021220L;
   private Item item;
   private ItemAttribute itemAttribute;

   public String toString() {
      return "ItemWithAttribute(item=" + this.getItem() + ", itemAttribute=" + this.getItemAttribute() + ")";
   }

   public Item getItem() {
      return this.item;
   }

   public void setItem(Item item) {
      this.item = item;
   }

   public ItemAttribute getItemAttribute() {
      return this.itemAttribute;
   }

   public void setItemAttribute(ItemAttribute itemAttribute) {
      this.itemAttribute = itemAttribute;
   }
}
