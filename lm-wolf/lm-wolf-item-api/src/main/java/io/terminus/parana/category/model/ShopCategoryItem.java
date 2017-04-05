package io.terminus.parana.category.model;

import java.io.Serializable;
import java.util.Date;

public class ShopCategoryItem implements Serializable {
   private static final long serialVersionUID = 0L;
   private Long id;
   private Long shopId;
   private Long itemId;
   private Long shopCategoryId;
   private Date createdAt;
   private Date updatedAt;

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ShopCategoryItem)) {
         return false;
      } else {
         ShopCategoryItem other = (ShopCategoryItem)o;
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

            Object this$itemId = this.getItemId();
            Object other$itemId = other.getItemId();
            if(this$itemId == null) {
               if(other$itemId != null) {
                  return false;
               }
            } else if(!this$itemId.equals(other$itemId)) {
               return false;
            }

            Object this$shopCategoryId = this.getShopCategoryId();
            Object other$shopCategoryId = other.getShopCategoryId();
            if(this$shopCategoryId == null) {
               if(other$shopCategoryId != null) {
                  return false;
               }
            } else if(!this$shopCategoryId.equals(other$shopCategoryId)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof ShopCategoryItem;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $shopId = this.getShopId();
      result = result * 59 + ($shopId == null?0:$shopId.hashCode());
      Object $itemId = this.getItemId();
      result = result * 59 + ($itemId == null?0:$itemId.hashCode());
      Object $shopCategoryId = this.getShopCategoryId();
      result = result * 59 + ($shopCategoryId == null?0:$shopCategoryId.hashCode());
      return result;
   }

   public String toString() {
      return "ShopCategoryItem(id=" + this.getId() + ", shopId=" + this.getShopId() + ", itemId=" + this.getItemId() + ", shopCategoryId=" + this.getShopCategoryId() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public Long getShopId() {
      return this.shopId;
   }

   public void setShopId(Long shopId) {
      this.shopId = shopId;
   }

   public Long getItemId() {
      return this.itemId;
   }

   public void setItemId(Long itemId) {
      this.itemId = itemId;
   }

   public Long getShopCategoryId() {
      return this.shopCategoryId;
   }

   public void setShopCategoryId(Long shopCategoryId) {
      this.shopCategoryId = shopCategoryId;
   }

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }
}
