package cn.blmdz.wolf.user.model;

import java.io.Serializable;
import java.util.Date;

public class SubDomain implements Serializable {
   private static final long serialVersionUID = 0L;
   private Long id;
   private String desc;
   private String value;
   private Integer type;
   private Long targetId;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public String getDesc() {
      return this.desc;
   }

   public String getValue() {
      return this.value;
   }

   public Integer getType() {
      return this.type;
   }

   public Long getTargetId() {
      return this.targetId;
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

   public void setDesc(String desc) {
      this.desc = desc;
   }

   public void setValue(String value) {
      this.value = value;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public void setTargetId(Long targetId) {
      this.targetId = targetId;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }

   public String toString() {
      return "SubDomain(id=" + this.getId() + ", desc=" + this.getDesc() + ", value=" + this.getValue() + ", type=" + this.getType() + ", targetId=" + this.getTargetId() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof SubDomain)) {
         return false;
      } else {
         SubDomain other = (SubDomain)o;
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

            Object this$value = this.getValue();
            Object other$value = other.getValue();
            if(this$value == null) {
               if(other$value != null) {
                  return false;
               }
            } else if(!this$value.equals(other$value)) {
               return false;
            }

            Object this$type = this.getType();
            Object other$type = other.getType();
            if(this$type == null) {
               if(other$type != null) {
                  return false;
               }
            } else if(!this$type.equals(other$type)) {
               return false;
            }

            Object this$targetId = this.getTargetId();
            Object other$targetId = other.getTargetId();
            if(this$targetId == null) {
               if(other$targetId != null) {
                  return false;
               }
            } else if(!this$targetId.equals(other$targetId)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof SubDomain;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $value = this.getValue();
      result = result * 31 + ($value == null?0:$value.hashCode());
      Object $type = this.getType();
      result = result * 31 + ($type == null?0:$type.hashCode());
      Object $targetId = this.getTargetId();
      result = result * 31 + ($targetId == null?0:$targetId.hashCode());
      return result;
   }
}
