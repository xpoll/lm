package cn.blmdz.wolf.attribute.dto;

import java.io.Serializable;

public class SkuAttribute implements Serializable {
   private static final long serialVersionUID = -4013141544103415365L;
   private String attrKey;
   private String attrVal;
   private String unit;
   private Boolean showImage;
   private String thumbnail;
   private String image;

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof SkuAttribute)) {
         return false;
      } else {
         SkuAttribute other = (SkuAttribute)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$attrKey = this.getAttrKey();
            Object other$attrKey = other.getAttrKey();
            if(this$attrKey == null) {
               if(other$attrKey != null) {
                  return false;
               }
            } else if(!this$attrKey.equals(other$attrKey)) {
               return false;
            }

            Object this$attrVal = this.getAttrVal();
            Object other$attrVal = other.getAttrVal();
            if(this$attrVal == null) {
               if(other$attrVal != null) {
                  return false;
               }
            } else if(!this$attrVal.equals(other$attrVal)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof SkuAttribute;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $attrKey = this.getAttrKey();
      result = result * 59 + ($attrKey == null?0:$attrKey.hashCode());
      Object $attrVal = this.getAttrVal();
      result = result * 59 + ($attrVal == null?0:$attrVal.hashCode());
      return result;
   }

   public String toString() {
      return "SkuAttribute(attrKey=" + this.getAttrKey() + ", attrVal=" + this.getAttrVal() + ", unit=" + this.getUnit() + ", showImage=" + this.getShowImage() + ", thumbnail=" + this.getThumbnail() + ", image=" + this.getImage() + ")";
   }

   public String getAttrKey() {
      return this.attrKey;
   }

   public void setAttrKey(String attrKey) {
      this.attrKey = attrKey;
   }

   public String getAttrVal() {
      return this.attrVal;
   }

   public void setAttrVal(String attrVal) {
      this.attrVal = attrVal;
   }

   public String getUnit() {
      return this.unit;
   }

   public void setUnit(String unit) {
      this.unit = unit;
   }

   public Boolean getShowImage() {
      return this.showImage;
   }

   public void setShowImage(Boolean showImage) {
      this.showImage = showImage;
   }

   public String getThumbnail() {
      return this.thumbnail;
   }

   public void setThumbnail(String thumbnail) {
      this.thumbnail = thumbnail;
   }

   public String getImage() {
      return this.image;
   }

   public void setImage(String image) {
      this.image = image;
   }
}
