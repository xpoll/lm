package cn.blmdz.wolf.item.dto;

import java.io.Serializable;

import cn.blmdz.wolf.item.model.Item;
import cn.blmdz.wolf.item.model.ItemAttribute;

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
