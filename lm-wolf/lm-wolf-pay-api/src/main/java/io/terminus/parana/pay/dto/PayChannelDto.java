package io.terminus.parana.pay.dto;

import java.io.Serializable;

public class PayChannelDto implements Serializable {
   private String name;
   private String description;
   private Boolean isChecked = Boolean.valueOf(false);

   public String getName() {
      return this.name;
   }

   public String getDescription() {
      return this.description;
   }

   public Boolean getIsChecked() {
      return this.isChecked;
   }

   public void setName(String name) {
      this.name = name;
   }

   public void setDescription(String description) {
      this.description = description;
   }

   public void setIsChecked(Boolean isChecked) {
      this.isChecked = isChecked;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof PayChannelDto)) {
         return false;
      } else {
         PayChannelDto other = (PayChannelDto)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$name = this.getName();
            Object other$name = other.getName();
            if(this$name == null) {
               if(other$name != null) {
                  return false;
               }
            } else if(!this$name.equals(other$name)) {
               return false;
            }

            Object this$description = this.getDescription();
            Object other$description = other.getDescription();
            if(this$description == null) {
               if(other$description != null) {
                  return false;
               }
            } else if(!this$description.equals(other$description)) {
               return false;
            }

            Object this$isChecked = this.getIsChecked();
            Object other$isChecked = other.getIsChecked();
            if(this$isChecked == null) {
               if(other$isChecked != null) {
                  return false;
               }
            } else if(!this$isChecked.equals(other$isChecked)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof PayChannelDto;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $name = this.getName();
      result = result * 31 + ($name == null?0:$name.hashCode());
      Object $description = this.getDescription();
      result = result * 31 + ($description == null?0:$description.hashCode());
      Object $isChecked = this.getIsChecked();
      result = result * 31 + ($isChecked == null?0:$isChecked.hashCode());
      return result;
   }

   public String toString() {
      return "PayChannelDto(name=" + this.getName() + ", description=" + this.getDescription() + ", isChecked=" + this.getIsChecked() + ")";
   }
}
