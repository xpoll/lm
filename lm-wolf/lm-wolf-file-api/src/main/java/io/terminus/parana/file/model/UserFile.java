package io.terminus.parana.file.model;

import java.io.Serializable;
import java.util.Date;

public class UserFile implements Serializable {
   private Long id;
   private Long createBy;
   private Integer fileType;
   private String group;
   private Long folderId;
   private String name;
   private String path;
   private Integer size;
   private String extra;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getCreateBy() {
      return this.createBy;
   }

   public Integer getFileType() {
      return this.fileType;
   }

   public String getGroup() {
      return this.group;
   }

   public Long getFolderId() {
      return this.folderId;
   }

   public String getName() {
      return this.name;
   }

   public String getPath() {
      return this.path;
   }

   public Integer getSize() {
      return this.size;
   }

   public String getExtra() {
      return this.extra;
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

   public void setFileType(Integer fileType) {
      this.fileType = fileType;
   }

   public void setGroup(String group) {
      this.group = group;
   }

   public void setFolderId(Long folderId) {
      this.folderId = folderId;
   }

   public void setName(String name) {
      this.name = name;
   }

   public void setPath(String path) {
      this.path = path;
   }

   public void setSize(Integer size) {
      this.size = size;
   }

   public void setExtra(String extra) {
      this.extra = extra;
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
      } else if(!(o instanceof UserFile)) {
         return false;
      } else {
         UserFile other = (UserFile)o;
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

            Object this$fileType = this.getFileType();
            Object other$fileType = other.getFileType();
            if(this$fileType == null) {
               if(other$fileType != null) {
                  return false;
               }
            } else if(!this$fileType.equals(other$fileType)) {
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

            Object this$folderId = this.getFolderId();
            Object other$folderId = other.getFolderId();
            if(this$folderId == null) {
               if(other$folderId != null) {
                  return false;
               }
            } else if(!this$folderId.equals(other$folderId)) {
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

            Object this$path = this.getPath();
            Object other$path = other.getPath();
            if(this$path == null) {
               if(other$path != null) {
                  return false;
               }
            } else if(!this$path.equals(other$path)) {
               return false;
            }

            Object this$size = this.getSize();
            Object other$size = other.getSize();
            if(this$size == null) {
               if(other$size != null) {
                  return false;
               }
            } else if(!this$size.equals(other$size)) {
               return false;
            }

            Object this$extra = this.getExtra();
            Object other$extra = other.getExtra();
            if(this$extra == null) {
               if(other$extra != null) {
                  return false;
               }
            } else if(!this$extra.equals(other$extra)) {
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
      return other instanceof UserFile;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $createBy = this.getCreateBy();
      result = result * 31 + ($createBy == null?0:$createBy.hashCode());
      Object $fileType = this.getFileType();
      result = result * 31 + ($fileType == null?0:$fileType.hashCode());
      Object $group = this.getGroup();
      result = result * 31 + ($group == null?0:$group.hashCode());
      Object $folderId = this.getFolderId();
      result = result * 31 + ($folderId == null?0:$folderId.hashCode());
      Object $name = this.getName();
      result = result * 31 + ($name == null?0:$name.hashCode());
      Object $path = this.getPath();
      result = result * 31 + ($path == null?0:$path.hashCode());
      Object $size = this.getSize();
      result = result * 31 + ($size == null?0:$size.hashCode());
      Object $extra = this.getExtra();
      result = result * 31 + ($extra == null?0:$extra.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "UserFile(id=" + this.getId() + ", createBy=" + this.getCreateBy() + ", fileType=" + this.getFileType() + ", group=" + this.getGroup() + ", folderId=" + this.getFolderId() + ", name=" + this.getName() + ", path=" + this.getPath() + ", size=" + this.getSize() + ", extra=" + this.getExtra() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
