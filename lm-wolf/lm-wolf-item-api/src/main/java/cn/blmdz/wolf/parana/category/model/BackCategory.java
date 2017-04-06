package cn.blmdz.wolf.parana.category.model;

import java.io.Serializable;
import java.util.Date;

public class BackCategory implements Serializable {
   private static final long serialVersionUID = -8511655074628678510L;
   private Long id;
   private Long pid;
   private String name;
   private Integer level;
   private Integer status;
   private Boolean hasChildren;
   private Boolean hasSpu;
   private String outerId;
   private Date createdAt;
   private Date updatedAt;

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof BackCategory)) {
         return false;
      } else {
         BackCategory other = (BackCategory)o;
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
      return other instanceof BackCategory;
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

   public String toString() {
      return "BackCategory(id=" + this.getId() + ", pid=" + this.getPid() + ", name=" + this.getName() + ", level=" + this.getLevel() + ", status=" + this.getStatus() + ", hasChildren=" + this.getHasChildren() + ", hasSpu=" + this.getHasSpu() + ", outerId=" + this.getOuterId() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
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

   public Integer getStatus() {
      return this.status;
   }

   public void setStatus(Integer status) {
      this.status = status;
   }

   public Boolean getHasChildren() {
      return this.hasChildren;
   }

   public void setHasChildren(Boolean hasChildren) {
      this.hasChildren = hasChildren;
   }

   public Boolean getHasSpu() {
      return this.hasSpu;
   }

   public void setHasSpu(Boolean hasSpu) {
      this.hasSpu = hasSpu;
   }

   public String getOuterId() {
      return this.outerId;
   }

   public void setOuterId(String outerId) {
      this.outerId = outerId;
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
