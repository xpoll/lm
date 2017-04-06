package cn.blmdz.wolf.auth.model;

import com.google.common.collect.Multimap;
import java.io.Serializable;
import java.util.List;

public class ExtendedRole implements Serializable {
   private static final long serialVersionUID = 0L;
   private String base;
   private String level;
   private List levelContext;
   private Multimap otherContext;

   public String getBase() {
      return this.base;
   }

   public String getLevel() {
      return this.level;
   }

   public List getLevelContext() {
      return this.levelContext;
   }

   public Multimap getOtherContext() {
      return this.otherContext;
   }

   public void setBase(String base) {
      this.base = base;
   }

   public void setLevel(String level) {
      this.level = level;
   }

   public void setLevelContext(List levelContext) {
      this.levelContext = levelContext;
   }

   public void setOtherContext(Multimap otherContext) {
      this.otherContext = otherContext;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ExtendedRole)) {
         return false;
      } else {
         ExtendedRole other = (ExtendedRole)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$base = this.getBase();
            Object other$base = other.getBase();
            if(this$base == null) {
               if(other$base != null) {
                  return false;
               }
            } else if(!this$base.equals(other$base)) {
               return false;
            }

            Object this$level = this.getLevel();
            Object other$level = other.getLevel();
            if(this$level == null) {
               if(other$level != null) {
                  return false;
               }
            } else if(!this$level.equals(other$level)) {
               return false;
            }

            Object this$levelContext = this.getLevelContext();
            Object other$levelContext = other.getLevelContext();
            if(this$levelContext == null) {
               if(other$levelContext != null) {
                  return false;
               }
            } else if(!this$levelContext.equals(other$levelContext)) {
               return false;
            }

            Object this$otherContext = this.getOtherContext();
            Object other$otherContext = other.getOtherContext();
            if(this$otherContext == null) {
               if(other$otherContext != null) {
                  return false;
               }
            } else if(!this$otherContext.equals(other$otherContext)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof ExtendedRole;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $base = this.getBase();
      result = result * 31 + ($base == null?0:$base.hashCode());
      Object $level = this.getLevel();
      result = result * 31 + ($level == null?0:$level.hashCode());
      Object $levelContext = this.getLevelContext();
      result = result * 31 + ($levelContext == null?0:$levelContext.hashCode());
      Object $otherContext = this.getOtherContext();
      result = result * 31 + ($otherContext == null?0:$otherContext.hashCode());
      return result;
   }

   public String toString() {
      return "ExtendedRole(base=" + this.getBase() + ", level=" + this.getLevel() + ", levelContext=" + this.getLevelContext() + ", otherContext=" + this.getOtherContext() + ")";
   }
}
