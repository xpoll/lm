package cn.blmdz.wolf.parana.search.dto;

import java.beans.ConstructorProperties;
import java.io.Serializable;
import java.util.List;

public class GroupedAggNav implements Serializable {
   private static final long serialVersionUID = -3630215454687536984L;
   private String group;
   private List nameAndCounts;

   public String getGroup() {
      return this.group;
   }

   public List getNameAndCounts() {
      return this.nameAndCounts;
   }

   public void setGroup(String group) {
      this.group = group;
   }

   public void setNameAndCounts(List nameAndCounts) {
      this.nameAndCounts = nameAndCounts;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof GroupedAggNav)) {
         return false;
      } else {
         GroupedAggNav other = (GroupedAggNav)o;
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

            Object this$nameAndCounts = this.getNameAndCounts();
            Object other$nameAndCounts = other.getNameAndCounts();
            if(this$nameAndCounts == null) {
               if(other$nameAndCounts != null) {
                  return false;
               }
            } else if(!this$nameAndCounts.equals(other$nameAndCounts)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof GroupedAggNav;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $group = this.getGroup();
      result = result * 59 + ($group == null?0:$group.hashCode());
      Object $nameAndCounts = this.getNameAndCounts();
      result = result * 59 + ($nameAndCounts == null?0:$nameAndCounts.hashCode());
      return result;
   }

   public String toString() {
      return "GroupedAggNav(group=" + this.getGroup() + ", nameAndCounts=" + this.getNameAndCounts() + ")";
   }

   public GroupedAggNav() {
   }

   @ConstructorProperties({"group", "nameAndCounts"})
   public GroupedAggNav(String group, List nameAndCounts) {
      this.group = group;
      this.nameAndCounts = nameAndCounts;
   }
}
