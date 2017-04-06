package cn.blmdz.wolf.file.model;

import java.io.Serializable;
import java.util.Date;

public class UserFolder implements Serializable {
   private Long id;
   private Long createBy;
   private String group;
   private Long pid;
   private Integer level;
   private Boolean hasChildren;
   private String folder;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getCreateBy() {
      return this.createBy;
   }

   public String getGroup() {
      return this.group;
   }

   public Long getPid() {
      return this.pid;
   }

   public Integer getLevel() {
      return this.level;
   }

   public Boolean getHasChildren() {
      return this.hasChildren;
   }

   public String getFolder() {
      return this.folder;
   }

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public void setCreateBy(Long createBy) {
      this.createBy = createBy;
   }

   public void setGroup(String group) {
      this.group = group;
   }

   public void setPid(Long pid) {
      this.pid = pid;
   }

   public void setLevel(Integer level) {
      this.level = level;
   }

   public void setHasChildren(Boolean hasChildren) {
      this.hasChildren = hasChildren;
   }

   public void setFolder(String folder) {
      this.folder = folder;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof UserFolder)) {
         return false;
      } else {
         UserFolder other = (UserFolder)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$id = this.getId();
            Object other$id = other.getId();
            if(this$id == null) {
               if(other$id != null) {
                  return false;
               }
            } else if(!this$id.equals(other$id)) {
               return false;
            }

            Object this$createBy = this.getCreateBy();
            Object other$createBy = other.getCreateBy();
            if(this$createBy == null) {
               if(other$createBy != null) {
                  return false;
               }
            } else if(!this$createBy.equals(other$createBy)) {
               return false;
            }

            Object this$group = this.getGroup();
            Object other$group = other.getGroup();
            if(this$group == null) {
               if(other$group != null) {
                  return false;
               }
            } else if(!this$group.equals(other$group)) {
               return false;
            }

            Object this$pid = this.getPid();
            Object other$pid = other.getPid();
            if(this$pid == null) {
               if(other$pid != null) {
                  return false;
               }
            } else if(!this$pid.equals(other$pid)) {
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

            Object this$hasChildren = this.getHasChildren();
            Object other$hasChildren = other.getHasChildren();
            if(this$hasChildren == null) {
               if(other$hasChildren != null) {
                  return false;
               }
            } else if(!this$hasChildren.equals(other$hasChildren)) {
               return false;
            }

            Object this$folder = this.getFolder();
            Object other$folder = other.getFolder();
            if(this$folder == null) {
               if(other$folder != null) {
                  return false;
               }
            } else if(!this$folder.equals(other$folder)) {
               return false;
            }

            Object this$createdAt = this.getCreatedAt();
            Object other$createdAt = other.getCreatedAt();
            if(this$createdAt == null) {
               if(other$createdAt != null) {
                  return false;
               }
            } else if(!this$createdAt.equals(other$createdAt)) {
               return false;
            }

            Object this$updatedAt = this.getUpdatedAt();
            Object other$updatedAt = other.getUpdatedAt();
            if(this$updatedAt == null) {
               if(other$updatedAt != null) {
                  return false;
               }
            } else if(!this$updatedAt.equals(other$updatedAt)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof UserFolder;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $createBy = this.getCreateBy();
      result = result * 31 + ($createBy == null?0:$createBy.hashCode());
      Object $group = this.getGroup();
      result = result * 31 + ($group == null?0:$group.hashCode());
      Object $pid = this.getPid();
      result = result * 31 + ($pid == null?0:$pid.hashCode());
      Object $level = this.getLevel();
      result = result * 31 + ($level == null?0:$level.hashCode());
      Object $hasChildren = this.getHasChildren();
      result = result * 31 + ($hasChildren == null?0:$hasChildren.hashCode());
      Object $folder = this.getFolder();
      result = result * 31 + ($folder == null?0:$folder.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "UserFolder(id=" + this.getId() + ", createBy=" + this.getCreateBy() + ", group=" + this.getGroup() + ", pid=" + this.getPid() + ", level=" + this.getLevel() + ", hasChildren=" + this.getHasChildren() + ", folder=" + this.getFolder() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
