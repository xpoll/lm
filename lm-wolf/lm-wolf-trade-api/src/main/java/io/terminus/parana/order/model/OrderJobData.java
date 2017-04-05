package io.terminus.parana.order.model;

import java.io.Serializable;
import java.util.Date;

public class OrderJobData implements Serializable {
   private static final long serialVersionUID = -8446449078999951295L;
   private Long id;
   private Long orderId;
   private Integer orderType;
   private Long actionInstanceId;
   private Integer status;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getOrderId() {
      return this.orderId;
   }

   public Integer getOrderType() {
      return this.orderType;
   }

   public Long getActionInstanceId() {
      return this.actionInstanceId;
   }

   public Integer getStatus() {
      return this.status;
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

   public void setOrderType(Integer orderType) {
      this.orderType = orderType;
   }

   public void setActionInstanceId(Long actionInstanceId) {
      this.actionInstanceId = actionInstanceId;
   }

   public void setStatus(Integer status) {
      this.status = status;
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
      } else if(!(o instanceof OrderJobData)) {
         return false;
      } else {
         OrderJobData other = (OrderJobData)o;
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

            Object this$orderType = this.getOrderType();
            Object other$orderType = other.getOrderType();
            if(this$orderType == null) {
               if(other$orderType != null) {
                  return false;
               }
            } else if(!this$orderType.equals(other$orderType)) {
               return false;
            }

            Object this$actionInstanceId = this.getActionInstanceId();
            Object other$actionInstanceId = other.getActionInstanceId();
            if(this$actionInstanceId == null) {
               if(other$actionInstanceId != null) {
                  return false;
               }
            } else if(!this$actionInstanceId.equals(other$actionInstanceId)) {
               return false;
            }

            Object this$status = this.getStatus();
            Object other$status = other.getStatus();
            if(this$status == null) {
               if(other$status != null) {
                  return false;
               }
            } else if(!this$status.equals(other$status)) {
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
      return other instanceof OrderJobData;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $id = this.getId();
      result = result * 59 + ($id == null?0:$id.hashCode());
      Object $orderId = this.getOrderId();
      result = result * 59 + ($orderId == null?0:$orderId.hashCode());
      Object $orderType = this.getOrderType();
      result = result * 59 + ($orderType == null?0:$orderType.hashCode());
      Object $actionInstanceId = this.getActionInstanceId();
      result = result * 59 + ($actionInstanceId == null?0:$actionInstanceId.hashCode());
      Object $status = this.getStatus();
      result = result * 59 + ($status == null?0:$status.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 59 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 59 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "OrderJobData(id=" + this.getId() + ", orderId=" + this.getOrderId() + ", orderType=" + this.getOrderType() + ", actionInstanceId=" + this.getActionInstanceId() + ", status=" + this.getStatus() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
