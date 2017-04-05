package io.terminus.parana.order.model;

import java.io.Serializable;
import java.util.Date;

public class OrderTransferRule implements Serializable {
   private static final long serialVersionUID = 6931088349286403821L;
   private Long id;
   private Long startNodeInstanceId;
   private Long endNodeInstanceId;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getStartNodeInstanceId() {
      return this.startNodeInstanceId;
   }

   public Long getEndNodeInstanceId() {
      return this.endNodeInstanceId;
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

   public void setStartNodeInstanceId(Long startNodeInstanceId) {
      this.startNodeInstanceId = startNodeInstanceId;
   }

   public void setEndNodeInstanceId(Long endNodeInstanceId) {
      this.endNodeInstanceId = endNodeInstanceId;
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
      } else if(!(o instanceof OrderTransferRule)) {
         return false;
      } else {
         OrderTransferRule other = (OrderTransferRule)o;
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

            Object this$startNodeInstanceId = this.getStartNodeInstanceId();
            Object other$startNodeInstanceId = other.getStartNodeInstanceId();
            if(this$startNodeInstanceId == null) {
               if(other$startNodeInstanceId != null) {
                  return false;
               }
            } else if(!this$startNodeInstanceId.equals(other$startNodeInstanceId)) {
               return false;
            }

            Object this$endNodeInstanceId = this.getEndNodeInstanceId();
            Object other$endNodeInstanceId = other.getEndNodeInstanceId();
            if(this$endNodeInstanceId == null) {
               if(other$endNodeInstanceId != null) {
                  return false;
               }
            } else if(!this$endNodeInstanceId.equals(other$endNodeInstanceId)) {
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
      return other instanceof OrderTransferRule;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $id = this.getId();
      result = result * 59 + ($id == null?0:$id.hashCode());
      Object $startNodeInstanceId = this.getStartNodeInstanceId();
      result = result * 59 + ($startNodeInstanceId == null?0:$startNodeInstanceId.hashCode());
      Object $endNodeInstanceId = this.getEndNodeInstanceId();
      result = result * 59 + ($endNodeInstanceId == null?0:$endNodeInstanceId.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 59 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 59 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "OrderTransferRule(id=" + this.getId() + ", startNodeInstanceId=" + this.getStartNodeInstanceId() + ", endNodeInstanceId=" + this.getEndNodeInstanceId() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
