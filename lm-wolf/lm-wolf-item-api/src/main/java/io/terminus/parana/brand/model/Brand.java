package io.terminus.parana.brand.model;

import java.io.Serializable;
import java.util.Date;

public class Brand implements Serializable {
   private static final long serialVersionUID = -3615566439002026797L;
   private Long id;
   private String name;
   private String enName;
   private String enCap;
   private String logo;
   private Integer status;
   private String description;
   private Date createdAt;
   private Date updatedAt;

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Brand)) {
         return false;
      } else {
         Brand other = (Brand)o;
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

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof Brand;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      return result;
   }

   public String toString() {
      return "Brand(name=" + this.getName() + ")";
   }

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public String getName() {
      return this.name;
   }

   public void setName(String name) {
      this.name = name;
   }

   public String getEnName() {
      return this.enName;
   }

   public void setEnName(String enName) {
      this.enName = enName;
   }

   public String getEnCap() {
      return this.enCap;
   }

   public void setEnCap(String enCap) {
      this.enCap = enCap;
   }

   public String getLogo() {
      return this.logo;
   }

   public void setLogo(String logo) {
      this.logo = logo;
   }

   public Integer getStatus() {
      return this.status;
   }

   public void setStatus(Integer status) {
      this.status = status;
   }

   public String getDescription() {
      return this.description;
   }

   public void setDescription(String description) {
      this.description = description;
   }

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }
}
