package io.terminus.parana.category.dto;

import java.io.Serializable;

public class ExchangeIndexDto implements Serializable {
   private static final long serialVersionUID = 4309909274133471011L;
   private Long categoryId;
   private Long attributeId1;
   private Long attributeId2;
   /** @deprecated */
   @Deprecated
   private Integer attributeId1Index;
   /** @deprecated */
   @Deprecated
   private Integer attributeId2Index;

   public Long getCategoryId() {
      return this.categoryId;
   }

   public Long getAttributeId1() {
      return this.attributeId1;
   }

   public Long getAttributeId2() {
      return this.attributeId2;
   }

   /** @deprecated */
   @Deprecated
   public Integer getAttributeId1Index() {
      return this.attributeId1Index;
   }

   /** @deprecated */
   @Deprecated
   public Integer getAttributeId2Index() {
      return this.attributeId2Index;
   }

   public void setCategoryId(Long categoryId) {
      this.categoryId = categoryId;
   }

   public void setAttributeId1(Long attributeId1) {
      this.attributeId1 = attributeId1;
   }

   public void setAttributeId2(Long attributeId2) {
      this.attributeId2 = attributeId2;
   }

   /** @deprecated */
   @Deprecated
   public void setAttributeId1Index(Integer attributeId1Index) {
      this.attributeId1Index = attributeId1Index;
   }

   /** @deprecated */
   @Deprecated
   public void setAttributeId2Index(Integer attributeId2Index) {
      this.attributeId2Index = attributeId2Index;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ExchangeIndexDto)) {
         return false;
      } else {
         ExchangeIndexDto other = (ExchangeIndexDto)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$categoryId = this.getCategoryId();
            Object other$categoryId = other.getCategoryId();
            if(this$categoryId == null) {
               if(other$categoryId != null) {
                  return false;
               }
            } else if(!this$categoryId.equals(other$categoryId)) {
               return false;
            }

            Object this$attributeId1 = this.getAttributeId1();
            Object other$attributeId1 = other.getAttributeId1();
            if(this$attributeId1 == null) {
               if(other$attributeId1 != null) {
                  return false;
               }
            } else if(!this$attributeId1.equals(other$attributeId1)) {
               return false;
            }

            Object this$attributeId2 = this.getAttributeId2();
            Object other$attributeId2 = other.getAttributeId2();
            if(this$attributeId2 == null) {
               if(other$attributeId2 != null) {
                  return false;
               }
            } else if(!this$attributeId2.equals(other$attributeId2)) {
               return false;
            }

            Object this$attributeId1Index = this.getAttributeId1Index();
            Object other$attributeId1Index = other.getAttributeId1Index();
            if(this$attributeId1Index == null) {
               if(other$attributeId1Index != null) {
                  return false;
               }
            } else if(!this$attributeId1Index.equals(other$attributeId1Index)) {
               return false;
            }

            Object this$attributeId2Index = this.getAttributeId2Index();
            Object other$attributeId2Index = other.getAttributeId2Index();
            if(this$attributeId2Index == null) {
               if(other$attributeId2Index != null) {
                  return false;
               }
            } else if(!this$attributeId2Index.equals(other$attributeId2Index)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof ExchangeIndexDto;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $categoryId = this.getCategoryId();
      result = result * 59 + ($categoryId == null?0:$categoryId.hashCode());
      Object $attributeId1 = this.getAttributeId1();
      result = result * 59 + ($attributeId1 == null?0:$attributeId1.hashCode());
      Object $attributeId2 = this.getAttributeId2();
      result = result * 59 + ($attributeId2 == null?0:$attributeId2.hashCode());
      Object $attributeId1Index = this.getAttributeId1Index();
      result = result * 59 + ($attributeId1Index == null?0:$attributeId1Index.hashCode());
      Object $attributeId2Index = this.getAttributeId2Index();
      result = result * 59 + ($attributeId2Index == null?0:$attributeId2Index.hashCode());
      return result;
   }

   public String toString() {
      return "ExchangeIndexDto(categoryId=" + this.getCategoryId() + ", attributeId1=" + this.getAttributeId1() + ", attributeId2=" + this.getAttributeId2() + ", attributeId1Index=" + this.getAttributeId1Index() + ", attributeId2Index=" + this.getAttributeId2Index() + ")";
   }
}
