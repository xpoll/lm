package cn.blmdz.wolf.parana.category.model;

import java.io.Serializable;
import java.util.Date;

public class FrontCategory implements Serializable {
   protected Long id;
   protected Long pid;
   protected String name;
   protected Integer level;
   protected Boolean hasChildren;
   protected String logo;
   private Date createdAt;
   private Date updatedAt;

   public String toString() {
      return "FrontCategory(id=" + this.getId() + ", pid=" + this.getPid() + ", name=" + this.getName() + ", level=" + this.getLevel() + ", hasChildren=" + this.getHasChildren() + ", logo=" + this.getLogo() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof FrontCategory)) {
         return false;
      } else {
         FrontCategory other = (FrontCategory)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$pid = this.getPid();
            Object other$pid = other.getPid();
            if(this$pid == null) {
               if(other$pid != null) {
                  return false;
               }
            } else if(!this$pid.equals(other$pid)) {
               return false;
            }

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
      return other instanceof FrontCategory;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $pid = this.getPid();
      result = result * 59 + ($pid == null?0:$pid.hashCode());
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      return result;
   }

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public Long getPid() {
      return this.pid;
   }

   public void setPid(Long pid) {
      this.pid = pid;
   }

   public String getName() {
      return this.name;
   }

   public void setName(String name) {
      this.name = name;
   }

   public Integer getLevel() {
      return this.level;
   }

   public void setLevel(Integer level) {
      this.level = level;
   }

   public Boolean getHasChildren() {
      return this.hasChildren;
   }

   public void setHasChildren(Boolean hasChildren) {
      this.hasChildren = hasChildren;
   }

   public String getLogo() {
      return this.logo;
   }

   public void setLogo(String logo) {
      this.logo = logo;
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
