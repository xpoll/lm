package cn.blmdz.boot.wolf.autoconfigure;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(
   prefix = "parana.config"
)
public class ConfigProperties {
   private String mode = "client";

   public String getMode() {
      return this.mode;
   }

   public void setMode(String mode) {
      this.mode = mode;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ConfigProperties)) {
         return false;
      } else {
         ConfigProperties other = (ConfigProperties)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$mode = this.getMode();
            Object other$mode = other.getMode();
            if(this$mode == null) {
               if(other$mode != null) {
                  return false;
               }
            } else if(!this$mode.equals(other$mode)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof ConfigProperties;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $mode = this.getMode();
      result = result * 31 + ($mode == null?0:$mode.hashCode());
      return result;
   }

   public String toString() {
      return "ConfigProperties(mode=" + this.getMode() + ")";
   }
}
