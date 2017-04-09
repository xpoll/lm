package cn.blmdz.wolf.attribute.dto;

import java.io.Serializable;

public class OtherAttribute implements Serializable {
   private static final long serialVersionUID = -1352745815416108996L;
   private String attrKey;
   private String attrVal;
   private String unit;
   private String group;
   private Boolean readOnlyBySeller;

   public String toString() {
      return "OtherAttribute(attrKey=" + this.getAttrKey() + ", attrVal=" + this.getAttrVal() + ", unit=" + this.getUnit() + ", group=" + this.getGroup() + ", readOnlyBySeller=" + this.getReadOnlyBySeller() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof OtherAttribute)) {
         return false;
      } else {
         OtherAttribute other = (OtherAttribute)o;
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
      return other instanceof OtherAttribute;
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

   public String getGroup() {
      return this.group;
   }

   public void setGroup(String group) {
      this.group = group;
   }

   public Boolean getReadOnlyBySeller() {
      return this.readOnlyBySeller;
   }

   public void setReadOnlyBySeller(Boolean readOnlyBySeller) {
      this.readOnlyBySeller = readOnlyBySeller;
   }
}
