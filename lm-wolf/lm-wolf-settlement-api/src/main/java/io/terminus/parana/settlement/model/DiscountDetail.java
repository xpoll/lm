package io.terminus.parana.settlement.model;

import java.io.Serializable;
import java.util.Date;

public class DiscountDetail implements Serializable {
   private static final long serialVersionUID = -8808792170166788132L;
   private Long id;
   private Long orderId;
   private Long discount;
   private Integer type;
   private String description;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getOrderId() {
      return this.orderId;
   }

   public Long getDiscount() {
      return this.discount;
   }

   public Integer getType() {
      return this.type;
   }

   public String getDescription() {
      return this.description;
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

   public void setOrderId(Long orderId) {
      this.orderId = orderId;
   }

   public void setDiscount(Long discount) {
      this.discount = discount;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public void setDescription(String description) {
      this.description = description;
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
      } else if(!(o instanceof DiscountDetail)) {
         return false;
      } else {
         DiscountDetail other = (DiscountDetail)o;
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

            Object this$orderId = this.getOrderId();
            Object other$orderId = other.getOrderId();
            if(this$orderId == null) {
               if(other$orderId != null) {
                  return false;
               }
            } else if(!this$orderId.equals(other$orderId)) {
               return false;
            }

            Object this$discount = this.getDiscount();
            Object other$discount = other.getDiscount();
            if(this$discount == null) {
               if(other$discount != null) {
                  return false;
               }
            } else if(!this$discount.equals(other$discount)) {
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

            Object this$description = this.getDescription();
            Object other$description = other.getDescription();
            if(this$description == null) {
               if(other$description != null) {
                  return false;
               }
            } else if(!this$description.equals(other$description)) {
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
      return other instanceof DiscountDetail;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $orderId = this.getOrderId();
      result = result * 31 + ($orderId == null?0:$orderId.hashCode());
      Object $discount = this.getDiscount();
      result = result * 31 + ($discount == null?0:$discount.hashCode());
      Object $type = this.getType();
      result = result * 31 + ($type == null?0:$type.hashCode());
      Object $description = this.getDescription();
      result = result * 31 + ($description == null?0:$description.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "DiscountDetail(id=" + this.getId() + ", orderId=" + this.getOrderId() + ", discount=" + this.getDiscount() + ", type=" + this.getType() + ", description=" + this.getDescription() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
