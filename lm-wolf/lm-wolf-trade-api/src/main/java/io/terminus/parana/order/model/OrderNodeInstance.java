package io.terminus.parana.order.model;

import java.io.Serializable;
import java.util.Date;

public class OrderNodeInstance implements Serializable {
   private static final long serialVersionUID = 2626544790466917699L;
   private Long id;
   private Long fid;
   private Boolean firstNode;
   private String names;
   private String desc;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getFid() {
      return this.fid;
   }

   public Boolean getFirstNode() {
      return this.firstNode;
   }

   public String getNames() {
      return this.names;
   }

   public String getDesc() {
      return this.desc;
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

   public void setFid(Long fid) {
      this.fid = fid;
   }

   public void setFirstNode(Boolean firstNode) {
      this.firstNode = firstNode;
   }

   public void setNames(String names) {
      this.names = names;
   }

   public void setDesc(String desc) {
      this.desc = desc;
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
      } else if(!(o instanceof OrderNodeInstance)) {
         return false;
      } else {
         OrderNodeInstance other = (OrderNodeInstance)o;
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

            Object this$fid = this.getFid();
            Object other$fid = other.getFid();
            if(this$fid == null) {
               if(other$fid != null) {
                  return false;
               }
            } else if(!this$fid.equals(other$fid)) {
               return false;
            }

            Object this$firstNode = this.getFirstNode();
            Object other$firstNode = other.getFirstNode();
            if(this$firstNode == null) {
               if(other$firstNode != null) {
                  return false;
               }
            } else if(!this$firstNode.equals(other$firstNode)) {
               return false;
            }

            Object this$names = this.getNames();
            Object other$names = other.getNames();
            if(this$names == null) {
               if(other$names != null) {
                  return false;
               }
            } else if(!this$names.equals(other$names)) {
               return false;
            }

            Object this$desc = this.getDesc();
            Object other$desc = other.getDesc();
            if(this$desc == null) {
               if(other$desc != null) {
                  return false;
               }
            } else if(!this$desc.equals(other$desc)) {
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

   protected boolean canEqual(Object other) {
      return other instanceof OrderNodeInstance;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $id = this.getId();
      result = result * 59 + ($id == null?0:$id.hashCode());
      Object $fid = this.getFid();
      result = result * 59 + ($fid == null?0:$fid.hashCode());
      Object $firstNode = this.getFirstNode();
      result = result * 59 + ($firstNode == null?0:$firstNode.hashCode());
      Object $names = this.getNames();
      result = result * 59 + ($names == null?0:$names.hashCode());
      Object $desc = this.getDesc();
      result = result * 59 + ($desc == null?0:$desc.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 59 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 59 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "OrderNodeInstance(id=" + this.getId() + ", fid=" + this.getFid() + ", firstNode=" + this.getFirstNode() + ", names=" + this.getNames() + ", desc=" + this.getDesc() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
