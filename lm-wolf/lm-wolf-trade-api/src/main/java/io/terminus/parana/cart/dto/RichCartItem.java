package io.terminus.parana.cart.dto;

import io.terminus.parana.cart.model.CartItem;
import io.terminus.parana.item.model.Sku;
import java.io.Serializable;

public class RichCartItem implements Serializable {
   private static final long serialVersionUID = 1537613794298226789L;
   private Sku sku;
   private CartItem cartItem;
   private String itemName;
   private String itemImage;
   private Integer itemStatus;

   public Sku getSku() {
      return this.sku;
   }

   public CartItem getCartItem() {
      return this.cartItem;
   }

   public String getItemName() {
      return this.itemName;
   }

   public String getItemImage() {
      return this.itemImage;
   }

   public Integer getItemStatus() {
      return this.itemStatus;
   }

   public void setSku(Sku sku) {
      this.sku = sku;
   }

   public void setCartItem(CartItem cartItem) {
      this.cartItem = cartItem;
   }

   public void setItemName(String itemName) {
      this.itemName = itemName;
   }

   public void setItemImage(String itemImage) {
      this.itemImage = itemImage;
   }

   public void setItemStatus(Integer itemStatus) {
      this.itemStatus = itemStatus;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof RichCartItem)) {
         return false;
      } else {
         RichCartItem other = (RichCartItem)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$sku = this.getSku();
            Object other$sku = other.getSku();
            if(this$sku == null) {
               if(other$sku != null) {
                  return false;
               }
            } else if(!this$sku.equals(other$sku)) {
               return false;
            }

            Object this$cartItem = this.getCartItem();
            Object other$cartItem = other.getCartItem();
            if(this$cartItem == null) {
               if(other$cartItem != null) {
                  return false;
               }
            } else if(!this$cartItem.equals(other$cartItem)) {
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

            Object this$itemImage = this.getItemImage();
            Object other$itemImage = other.getItemImage();
            if(this$itemImage == null) {
               if(other$itemImage != null) {
                  return false;
               }
            } else if(!this$itemImage.equals(other$itemImage)) {
               return false;
            }

            Object this$itemStatus = this.getItemStatus();
            Object other$itemStatus = other.getItemStatus();
            if(this$itemStatus == null) {
               if(other$itemStatus != null) {
                  return false;
               }
            } else if(!this$itemStatus.equals(other$itemStatus)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof RichCartItem;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $sku = this.getSku();
      result = result * 59 + ($sku == null?0:$sku.hashCode());
      Object $cartItem = this.getCartItem();
      result = result * 59 + ($cartItem == null?0:$cartItem.hashCode());
      Object $itemName = this.getItemName();
      result = result * 59 + ($itemName == null?0:$itemName.hashCode());
      Object $itemImage = this.getItemImage();
      result = result * 59 + ($itemImage == null?0:$itemImage.hashCode());
      Object $itemStatus = this.getItemStatus();
      result = result * 59 + ($itemStatus == null?0:$itemStatus.hashCode());
      return result;
   }

   public String toString() {
      return "RichCartItem(sku=" + this.getSku() + ", cartItem=" + this.getCartItem() + ", itemName=" + this.getItemName() + ", itemImage=" + this.getItemImage() + ", itemStatus=" + this.getItemStatus() + ")";
   }
}
