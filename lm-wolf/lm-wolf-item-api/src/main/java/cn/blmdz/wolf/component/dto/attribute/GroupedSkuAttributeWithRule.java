package cn.blmdz.wolf.component.dto.attribute;

import java.io.Serializable;
import java.util.List;

public class GroupedSkuAttributeWithRule implements Serializable {
   private static final long serialVersionUID = 2227166424258434936L;
   private String attrKey;
   private List attributeRules;

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof GroupedSkuAttributeWithRule)) {
         return false;
      } else {
         GroupedSkuAttributeWithRule other = (GroupedSkuAttributeWithRule)o;
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
      return other instanceof GroupedSkuAttributeWithRule;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $attrKey = this.getAttrKey();
      result = result * 59 + ($attrKey == null?0:$attrKey.hashCode());
      return result;
   }

   public String getAttrKey() {
      return this.attrKey;
   }

   public void setAttrKey(String attrKey) {
      this.attrKey = attrKey;
   }

   public List getAttributeRules() {
      return this.attributeRules;
   }

   public void setAttributeRules(List attributeRules) {
      this.attributeRules = attributeRules;
   }
}
