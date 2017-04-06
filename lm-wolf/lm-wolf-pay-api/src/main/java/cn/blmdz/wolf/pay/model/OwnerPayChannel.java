package cn.blmdz.wolf.pay.model;

import java.io.Serializable;
import java.util.Date;

public class OwnerPayChannel implements Serializable {
   private static final long serialVersionUID = -5314404418585707864L;
   private Long id;
   private Long ownerId;
   private String ownerName;
   private Integer type;
   private String channel;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getOwnerId() {
      return this.ownerId;
   }

   public String getOwnerName() {
      return this.ownerName;
   }

   public Integer getType() {
      return this.type;
   }

   public String getChannel() {
      return this.channel;
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

   public void setOwnerId(Long ownerId) {
      this.ownerId = ownerId;
   }

   public void setOwnerName(String ownerName) {
      this.ownerName = ownerName;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public void setChannel(String channel) {
      this.channel = channel;
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
      } else if(!(o instanceof OwnerPayChannel)) {
         return false;
      } else {
         OwnerPayChannel other = (OwnerPayChannel)o;
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

            Object this$ownerId = this.getOwnerId();
            Object other$ownerId = other.getOwnerId();
            if(this$ownerId == null) {
               if(other$ownerId != null) {
                  return false;
               }
            } else if(!this$ownerId.equals(other$ownerId)) {
               return false;
            }

            Object this$ownerName = this.getOwnerName();
            Object other$ownerName = other.getOwnerName();
            if(this$ownerName == null) {
               if(other$ownerName != null) {
                  return false;
               }
            } else if(!this$ownerName.equals(other$ownerName)) {
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

            Object this$channel = this.getChannel();
            Object other$channel = other.getChannel();
            if(this$channel == null) {
               if(other$channel != null) {
                  return false;
               }
            } else if(!this$channel.equals(other$channel)) {
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
      return other instanceof OwnerPayChannel;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $ownerId = this.getOwnerId();
      result = result * 31 + ($ownerId == null?0:$ownerId.hashCode());
      Object $ownerName = this.getOwnerName();
      result = result * 31 + ($ownerName == null?0:$ownerName.hashCode());
      Object $type = this.getType();
      result = result * 31 + ($type == null?0:$type.hashCode());
      Object $channel = this.getChannel();
      result = result * 31 + ($channel == null?0:$channel.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "OwnerPayChannel(id=" + this.getId() + ", ownerId=" + this.getOwnerId() + ", ownerName=" + this.getOwnerName() + ", type=" + this.getType() + ", channel=" + this.getChannel() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
