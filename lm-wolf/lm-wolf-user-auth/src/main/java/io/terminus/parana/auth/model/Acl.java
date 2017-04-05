package io.terminus.parana.auth.model;

import java.io.Serializable;
import java.util.Map;

public class Acl implements Serializable {
   private static final long serialVersionUID = 0L;
   private Map defaults;
   private Map resources;
   private Map trees;

   public Map getDefaults() {
      return this.defaults;
   }

   public Map getResources() {
      return this.resources;
   }

   public Map getTrees() {
      return this.trees;
   }

   public void setDefaults(Map defaults) {
      this.defaults = defaults;
   }

   public void setResources(Map resources) {
      this.resources = resources;
   }

   public void setTrees(Map trees) {
      this.trees = trees;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Acl)) {
         return false;
      } else {
         Acl other = (Acl)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$defaults = this.getDefaults();
            Object other$defaults = other.getDefaults();
            if(this$defaults == null) {
               if(other$defaults != null) {
                  return false;
               }
            } else if(!this$defaults.equals(other$defaults)) {
               return false;
            }

            Object this$resources = this.getResources();
            Object other$resources = other.getResources();
            if(this$resources == null) {
               if(other$resources != null) {
                  return false;
               }
            } else if(!this$resources.equals(other$resources)) {
               return false;
            }

            Object this$trees = this.getTrees();
            Object other$trees = other.getTrees();
            if(this$trees == null) {
               if(other$trees != null) {
                  return false;
               }
            } else if(!this$trees.equals(other$trees)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof Acl;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $defaults = this.getDefaults();
      result = result * 31 + ($defaults == null?0:$defaults.hashCode());
      Object $resources = this.getResources();
      result = result * 31 + ($resources == null?0:$resources.hashCode());
      Object $trees = this.getTrees();
      result = result * 31 + ($trees == null?0:$trees.hashCode());
      return result;
   }

   public String toString() {
      return "Acl(defaults=" + this.getDefaults() + ", resources=" + this.getResources() + ", trees=" + this.getTrees() + ")";
   }
}
