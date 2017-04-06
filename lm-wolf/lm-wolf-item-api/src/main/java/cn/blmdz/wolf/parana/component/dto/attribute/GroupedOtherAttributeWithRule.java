package cn.blmdz.wolf.parana.component.dto.attribute;

import java.io.Serializable;
import java.util.List;

public class GroupedOtherAttributeWithRule implements Serializable {
   private static final long serialVersionUID = -8642674415025148423L;
   private String group;
   private List otherAttributeWithRules;

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof GroupedOtherAttributeWithRule)) {
         return false;
      } else {
         GroupedOtherAttributeWithRule other = (GroupedOtherAttributeWithRule)o;
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
      return other instanceof GroupedOtherAttributeWithRule;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $group = this.getGroup();
      result = result * 59 + ($group == null?0:$group.hashCode());
      return result;
   }

   public String getGroup() {
      return this.group;
   }

   public void setGroup(String group) {
      this.group = group;
   }

   public List getOtherAttributeWithRules() {
      return this.otherAttributeWithRules;
   }

   public void setOtherAttributeWithRules(List otherAttributeWithRules) {
      this.otherAttributeWithRules = otherAttributeWithRules;
   }
}
