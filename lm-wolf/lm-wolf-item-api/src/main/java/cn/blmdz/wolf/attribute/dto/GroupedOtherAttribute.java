package cn.blmdz.wolf.attribute.dto;

import java.beans.ConstructorProperties;
import java.io.Serializable;
import java.util.List;

public class GroupedOtherAttribute implements Serializable {
   private static final long serialVersionUID = 5426128494192982703L;
   private String group;
   private List<OtherAttribute> otherAttributes;

   public String toString() {
      return "GroupedOtherAttribute(group=" + this.getGroup() + ", otherAttributes=" + this.getOtherAttributes() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof GroupedOtherAttribute)) {
         return false;
      } else {
         GroupedOtherAttribute other = (GroupedOtherAttribute)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$group = this.getGroup();
            Object other$group = other.getGroup();
            if(this$group == null) {
               if(other$group != null) {
                  return false;
               }
            } else if(!this$group.equals(other$group)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof GroupedOtherAttribute;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $group = this.getGroup();
      result = result * 59 + ($group == null?0:$group.hashCode());
      return result;
   }

   public GroupedOtherAttribute() {
   }

   @ConstructorProperties({"group", "otherAttributes"})
   public GroupedOtherAttribute(String group, List otherAttributes) {
      this.group = group;
      this.otherAttributes = otherAttributes;
   }

   public String getGroup() {
      return this.group;
   }

   public void setGroup(String group) {
      this.group = group;
   }

   public List<OtherAttribute> getOtherAttributes() {
      return this.otherAttributes;
   }

   public void setOtherAttributes(List otherAttributes) {
      this.otherAttributes = otherAttributes;
   }
}
