package cn.blmdz.wolf.attribute.dto;

import java.beans.ConstructorProperties;
import java.io.Serializable;
import java.util.List;

public class GroupedSkuAttribute implements Serializable {
   private static final long serialVersionUID = 4573776002497222184L;
   private String attrKey;
   private List<SkuAttribute> skuAttributes;

   public String toString() {
      return "GroupedSkuAttribute(attrKey=" + this.getAttrKey() + ", skuAttributes=" + this.getSkuAttributes() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof GroupedSkuAttribute)) {
         return false;
      } else {
         GroupedSkuAttribute other = (GroupedSkuAttribute)o;
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

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof GroupedSkuAttribute;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $attrKey = this.getAttrKey();
      result = result * 59 + ($attrKey == null?0:$attrKey.hashCode());
      return result;
   }

   public GroupedSkuAttribute() {
   }

   @ConstructorProperties({"attrKey", "skuAttributes"})
   public GroupedSkuAttribute(String attrKey, List skuAttributes) {
      this.attrKey = attrKey;
      this.skuAttributes = skuAttributes;
   }

   public String getAttrKey() {
      return this.attrKey;
   }

   public void setAttrKey(String attrKey) {
      this.attrKey = attrKey;
   }

   public List<SkuAttribute> getSkuAttributes() {
      return this.skuAttributes;
   }

   public void setSkuAttributes(List skuAttributes) {
      this.skuAttributes = skuAttributes;
   }
}
