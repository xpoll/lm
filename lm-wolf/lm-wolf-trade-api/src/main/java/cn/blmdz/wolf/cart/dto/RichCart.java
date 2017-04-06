package cn.blmdz.wolf.cart.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import cn.blmdz.home.common.util.Arguments;

public class RichCart implements Serializable, Comparable<RichCart> {
   private static final long serialVersionUID = -1162570374269310683L;
   private Long shopId;
   private String shopName;
   private Long sellerId;
   private String shopImage;
   private List cartItems;
   private Date updatedAt;

   public void flushUpdateAt() {
      if(Arguments.isNullOrEmpty(this.cartItems)) {
         this.updatedAt = null;
      } else {
         this.updatedAt = ((RichCartItem)this.cartItems.get(0)).getCartItem().getUpdatedAt();
      }
   }

   public int compareTo(RichCart o) {
      long that = o.getUpdatedAt().getTime();
      long self = this.updatedAt.getTime();
      return self > that?-1:(self < that?1:0);
   }

   public Long getShopId() {
      return this.shopId;
   }

   public String getShopName() {
      return this.shopName;
   }

   public Long getSellerId() {
      return this.sellerId;
   }

   public String getShopImage() {
      return this.shopImage;
   }

   public List getCartItems() {
      return this.cartItems;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setShopId(Long shopId) {
      this.shopId = shopId;
   }

   public void setShopName(String shopName) {
      this.shopName = shopName;
   }

   public void setSellerId(Long sellerId) {
      this.sellerId = sellerId;
   }

   public void setShopImage(String shopImage) {
      this.shopImage = shopImage;
   }

   public void setCartItems(List cartItems) {
      this.cartItems = cartItems;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof RichCart)) {
         return false;
      } else {
         RichCart other = (RichCart)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$shopId = this.getShopId();
            Object other$shopId = other.getShopId();
            if(this$shopId == null) {
               if(other$shopId != null) {
                  return false;
               }
            } else if(!this$shopId.equals(other$shopId)) {
               return false;
            }

            Object this$shopName = this.getShopName();
            Object other$shopName = other.getShopName();
            if(this$shopName == null) {
               if(other$shopName != null) {
                  return false;
               }
            } else if(!this$shopName.equals(other$shopName)) {
               return false;
            }

            Object this$sellerId = this.getSellerId();
            Object other$sellerId = other.getSellerId();
            if(this$sellerId == null) {
               if(other$sellerId != null) {
                  return false;
               }
            } else if(!this$sellerId.equals(other$sellerId)) {
               return false;
            }

            Object this$shopImage = this.getShopImage();
            Object other$shopImage = other.getShopImage();
            if(this$shopImage == null) {
               if(other$shopImage != null) {
                  return false;
               }
            } else if(!this$shopImage.equals(other$shopImage)) {
               return false;
            }

            Object this$cartItems = this.getCartItems();
            Object other$cartItems = other.getCartItems();
            if(this$cartItems == null) {
               if(other$cartItems != null) {
                  return false;
               }
            } else if(!this$cartItems.equals(other$cartItems)) {
               return false;
            }

            Object this$updatedAt = this.getUpdatedAt();
            Object other$updatedAt = other.getUpdatedAt();
            if(this$updatedAt == null) {
               if(other$updatedAt != null) {
                  return false;
               }
            } else if(!this$updatedAt.equals(other$updatedAt)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof RichCart;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $shopId = this.getShopId();
      result = result * 59 + ($shopId == null?0:$shopId.hashCode());
      Object $shopName = this.getShopName();
      result = result * 59 + ($shopName == null?0:$shopName.hashCode());
      Object $sellerId = this.getSellerId();
      result = result * 59 + ($sellerId == null?0:$sellerId.hashCode());
      Object $shopImage = this.getShopImage();
      result = result * 59 + ($shopImage == null?0:$shopImage.hashCode());
      Object $cartItems = this.getCartItems();
      result = result * 59 + ($cartItems == null?0:$cartItems.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 59 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "RichCart(shopId=" + this.getShopId() + ", shopName=" + this.getShopName() + ", sellerId=" + this.getSellerId() + ", shopImage=" + this.getShopImage() + ", cartItems=" + this.getCartItems() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
