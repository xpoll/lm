package io.terminus.parana.search.dto;

import java.io.Serializable;

public class SearchedItem implements Serializable {
   private static final long serialVersionUID = 6486052625288623389L;
   private Long id;
   private String name;
   private String mainImage;
   private Integer price;
   private Long shopId;
   private String shopName;
   private Integer saleQuantity;

   public Long getId() {
      return this.id;
   }

   public String getName() {
      return this.name;
   }

   public String getMainImage() {
      return this.mainImage;
   }

   public Integer getPrice() {
      return this.price;
   }

   public Long getShopId() {
      return this.shopId;
   }

   public String getShopName() {
      return this.shopName;
   }

   public Integer getSaleQuantity() {
      return this.saleQuantity;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public void setName(String name) {
      this.name = name;
   }

   public void setMainImage(String mainImage) {
      this.mainImage = mainImage;
   }

   public void setPrice(Integer price) {
      this.price = price;
   }

   public void setShopId(Long shopId) {
      this.shopId = shopId;
   }

   public void setShopName(String shopName) {
      this.shopName = shopName;
   }

   public void setSaleQuantity(Integer saleQuantity) {
      this.saleQuantity = saleQuantity;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof SearchedItem)) {
         return false;
      } else {
         SearchedItem other = (SearchedItem)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$id = this.getId();
            Object other$id = other.getId();
            if(this$id == null) {
               if(other$id != null) {
                  return false;
               }
            } else if(!this$id.equals(other$id)) {
               return false;
            }

            Object this$name = this.getName();
            Object other$name = other.getName();
            if(this$name == null) {
               if(other$name != null) {
                  return false;
               }
            } else if(!this$name.equals(other$name)) {
               return false;
            }

            Object this$mainImage = this.getMainImage();
            Object other$mainImage = other.getMainImage();
            if(this$mainImage == null) {
               if(other$mainImage != null) {
                  return false;
               }
            } else if(!this$mainImage.equals(other$mainImage)) {
               return false;
            }

            Object this$price = this.getPrice();
            Object other$price = other.getPrice();
            if(this$price == null) {
               if(other$price != null) {
                  return false;
               }
            } else if(!this$price.equals(other$price)) {
               return false;
            }

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

            Object this$saleQuantity = this.getSaleQuantity();
            Object other$saleQuantity = other.getSaleQuantity();
            if(this$saleQuantity == null) {
               if(other$saleQuantity != null) {
                  return false;
               }
            } else if(!this$saleQuantity.equals(other$saleQuantity)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof SearchedItem;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $id = this.getId();
      result = result * 59 + ($id == null?0:$id.hashCode());
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      Object $mainImage = this.getMainImage();
      result = result * 59 + ($mainImage == null?0:$mainImage.hashCode());
      Object $price = this.getPrice();
      result = result * 59 + ($price == null?0:$price.hashCode());
      Object $shopId = this.getShopId();
      result = result * 59 + ($shopId == null?0:$shopId.hashCode());
      Object $shopName = this.getShopName();
      result = result * 59 + ($shopName == null?0:$shopName.hashCode());
      Object $saleQuantity = this.getSaleQuantity();
      result = result * 59 + ($saleQuantity == null?0:$saleQuantity.hashCode());
      return result;
   }

   public String toString() {
      return "SearchedItem(id=" + this.getId() + ", name=" + this.getName() + ", mainImage=" + this.getMainImage() + ", price=" + this.getPrice() + ", shopId=" + this.getShopId() + ", shopName=" + this.getShopName() + ", saleQuantity=" + this.getSaleQuantity() + ")";
   }
}
