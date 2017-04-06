package cn.blmdz.wolf.parana.rule.dto;

import java.io.Serializable;
import java.util.List;

public abstract class BaseOutput implements Serializable {
   private static final long serialVersionUID = -1434623015265583711L;
   protected List skuAttrs;
   protected List otherAttrs;

   public abstract void setGeneralSku(List var1);

   public List getSkuAttrs() {
      return this.skuAttrs;
   }

   public List getOtherAttrs() {
      return this.otherAttrs;
   }

   public void setSkuAttrs(List skuAttrs) {
      this.skuAttrs = skuAttrs;
   }

   public void setOtherAttrs(List otherAttrs) {
      this.otherAttrs = otherAttrs;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof BaseOutput)) {
         return false;
      } else {
         BaseOutput other = (BaseOutput)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$skuAttrs = this.getSkuAttrs();
            Object other$skuAttrs = other.getSkuAttrs();
            if(this$skuAttrs == null) {
               if(other$skuAttrs != null) {
                  return false;
               }
            } else if(!this$skuAttrs.equals(other$skuAttrs)) {
               return false;
            }

            Object this$otherAttrs = this.getOtherAttrs();
            Object other$otherAttrs = other.getOtherAttrs();
            if(this$otherAttrs == null) {
               if(other$otherAttrs != null) {
                  return false;
               }
            } else if(!this$otherAttrs.equals(other$otherAttrs)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof BaseOutput;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $skuAttrs = this.getSkuAttrs();
      result = result * 59 + ($skuAttrs == null?0:$skuAttrs.hashCode());
      Object $otherAttrs = this.getOtherAttrs();
      result = result * 59 + ($otherAttrs == null?0:$otherAttrs.hashCode());
      return result;
   }

   public String toString() {
      return "BaseOutput(skuAttrs=" + this.getSkuAttrs() + ", otherAttrs=" + this.getOtherAttrs() + ")";
   }
}
