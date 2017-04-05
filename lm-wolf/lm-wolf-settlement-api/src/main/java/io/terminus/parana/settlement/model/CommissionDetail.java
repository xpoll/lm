package io.terminus.parana.settlement.model;

import java.io.Serializable;
import java.util.Date;

public class CommissionDetail implements Serializable {
   private static final long serialVersionUID = 6413339359131111153L;
   private Long id;
   private Long orderId;
   private Long commissionRuleId;
   private Long commission;
   private Long price;
   private Long lowestFee;
   private Integer rate;
   private Integer type;
   private String description;
   private String systemNo;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getOrderId() {
      return this.orderId;
   }

   public Long getCommissionRuleId() {
      return this.commissionRuleId;
   }

   public Long getCommission() {
      return this.commission;
   }

   public Long getPrice() {
      return this.price;
   }

   public Long getLowestFee() {
      return this.lowestFee;
   }

   public Integer getRate() {
      return this.rate;
   }

   public Integer getType() {
      return this.type;
   }

   public String getDescription() {
      return this.description;
   }

   public String getSystemNo() {
      return this.systemNo;
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

   public void setCommissionRuleId(Long commissionRuleId) {
      this.commissionRuleId = commissionRuleId;
   }

   public void setCommission(Long commission) {
      this.commission = commission;
   }

   public void setPrice(Long price) {
      this.price = price;
   }

   public void setLowestFee(Long lowestFee) {
      this.lowestFee = lowestFee;
   }

   public void setRate(Integer rate) {
      this.rate = rate;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public void setDescription(String description) {
      this.description = description;
   }

   public void setSystemNo(String systemNo) {
      this.systemNo = systemNo;
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
      } else if(!(o instanceof CommissionDetail)) {
         return false;
      } else {
         CommissionDetail other = (CommissionDetail)o;
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

            Object this$commissionRuleId = this.getCommissionRuleId();
            Object other$commissionRuleId = other.getCommissionRuleId();
            if(this$commissionRuleId == null) {
               if(other$commissionRuleId != null) {
                  return false;
               }
            } else if(!this$commissionRuleId.equals(other$commissionRuleId)) {
               return false;
            }

            Object this$commission = this.getCommission();
            Object other$commission = other.getCommission();
            if(this$commission == null) {
               if(other$commission != null) {
                  return false;
               }
            } else if(!this$commission.equals(other$commission)) {
               return false;
            }

            Object this$price = this.getPrice();
            Object other$price = other.getPrice();
            if(this$price == null) {
               if(other$price != null) {
                  return false;
               }
            } else if(!this$price.equals(other$price)) {
               return false;
            }

            Object this$lowestFee = this.getLowestFee();
            Object other$lowestFee = other.getLowestFee();
            if(this$lowestFee == null) {
               if(other$lowestFee != null) {
                  return false;
               }
            } else if(!this$lowestFee.equals(other$lowestFee)) {
               return false;
            }

            Object this$rate = this.getRate();
            Object other$rate = other.getRate();
            if(this$rate == null) {
               if(other$rate != null) {
                  return false;
               }
            } else if(!this$rate.equals(other$rate)) {
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

            Object this$systemNo = this.getSystemNo();
            Object other$systemNo = other.getSystemNo();
            if(this$systemNo == null) {
               if(other$systemNo != null) {
                  return false;
               }
            } else if(!this$systemNo.equals(other$systemNo)) {
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
      return other instanceof CommissionDetail;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $orderId = this.getOrderId();
      result = result * 31 + ($orderId == null?0:$orderId.hashCode());
      Object $commissionRuleId = this.getCommissionRuleId();
      result = result * 31 + ($commissionRuleId == null?0:$commissionRuleId.hashCode());
      Object $commission = this.getCommission();
      result = result * 31 + ($commission == null?0:$commission.hashCode());
      Object $price = this.getPrice();
      result = result * 31 + ($price == null?0:$price.hashCode());
      Object $lowestFee = this.getLowestFee();
      result = result * 31 + ($lowestFee == null?0:$lowestFee.hashCode());
      Object $rate = this.getRate();
      result = result * 31 + ($rate == null?0:$rate.hashCode());
      Object $type = this.getType();
      result = result * 31 + ($type == null?0:$type.hashCode());
      Object $description = this.getDescription();
      result = result * 31 + ($description == null?0:$description.hashCode());
      Object $systemNo = this.getSystemNo();
      result = result * 31 + ($systemNo == null?0:$systemNo.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "CommissionDetail(id=" + this.getId() + ", orderId=" + this.getOrderId() + ", commissionRuleId=" + this.getCommissionRuleId() + ", commission=" + this.getCommission() + ", price=" + this.getPrice() + ", lowestFee=" + this.getLowestFee() + ", rate=" + this.getRate() + ", type=" + this.getType() + ", description=" + this.getDescription() + ", systemNo=" + this.getSystemNo() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
