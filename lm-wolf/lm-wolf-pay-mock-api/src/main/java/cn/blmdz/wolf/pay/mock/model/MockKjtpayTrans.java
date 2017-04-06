package cn.blmdz.wolf.pay.mock.model;

import java.io.Serializable;
import java.util.Date;

public class MockKjtpayTrans implements Serializable {
   private static final long serialVersionUID = 6637073795995965472L;
   private Long id;
   private String outerNo;
   private String origOuterNo;
   private String innerNo;
   private String type;
   private Date orderAt;
   private Date paidAt;
   private String amount;
   private String rate;
   private String rateFee;
   private String status;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public String getOuterNo() {
      return this.outerNo;
   }

   public String getOrigOuterNo() {
      return this.origOuterNo;
   }

   public String getInnerNo() {
      return this.innerNo;
   }

   public String getType() {
      return this.type;
   }

   public Date getOrderAt() {
      return this.orderAt;
   }

   public Date getPaidAt() {
      return this.paidAt;
   }

   public String getAmount() {
      return this.amount;
   }

   public String getRate() {
      return this.rate;
   }

   public String getRateFee() {
      return this.rateFee;
   }

   public String getStatus() {
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

   public void setOuterNo(String outerNo) {
      this.outerNo = outerNo;
   }

   public void setOrigOuterNo(String origOuterNo) {
      this.origOuterNo = origOuterNo;
   }

   public void setInnerNo(String innerNo) {
      this.innerNo = innerNo;
   }

   public void setType(String type) {
      this.type = type;
   }

   public void setOrderAt(Date orderAt) {
      this.orderAt = orderAt;
   }

   public void setPaidAt(Date paidAt) {
      this.paidAt = paidAt;
   }

   public void setAmount(String amount) {
      this.amount = amount;
   }

   public void setRate(String rate) {
      this.rate = rate;
   }

   public void setRateFee(String rateFee) {
      this.rateFee = rateFee;
   }

   public void setStatus(String status) {
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
      } else if(!(o instanceof MockKjtpayTrans)) {
         return false;
      } else {
         MockKjtpayTrans other = (MockKjtpayTrans)o;
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

            Object this$outerNo = this.getOuterNo();
            Object other$outerNo = other.getOuterNo();
            if(this$outerNo == null) {
               if(other$outerNo != null) {
                  return false;
               }
            } else if(!this$outerNo.equals(other$outerNo)) {
               return false;
            }

            Object this$origOuterNo = this.getOrigOuterNo();
            Object other$origOuterNo = other.getOrigOuterNo();
            if(this$origOuterNo == null) {
               if(other$origOuterNo != null) {
                  return false;
               }
            } else if(!this$origOuterNo.equals(other$origOuterNo)) {
               return false;
            }

            Object this$innerNo = this.getInnerNo();
            Object other$innerNo = other.getInnerNo();
            if(this$innerNo == null) {
               if(other$innerNo != null) {
                  return false;
               }
            } else if(!this$innerNo.equals(other$innerNo)) {
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

            Object this$orderAt = this.getOrderAt();
            Object other$orderAt = other.getOrderAt();
            if(this$orderAt == null) {
               if(other$orderAt != null) {
                  return false;
               }
            } else if(!this$orderAt.equals(other$orderAt)) {
               return false;
            }

            Object this$paidAt = this.getPaidAt();
            Object other$paidAt = other.getPaidAt();
            if(this$paidAt == null) {
               if(other$paidAt != null) {
                  return false;
               }
            } else if(!this$paidAt.equals(other$paidAt)) {
               return false;
            }

            Object this$amount = this.getAmount();
            Object other$amount = other.getAmount();
            if(this$amount == null) {
               if(other$amount != null) {
                  return false;
               }
            } else if(!this$amount.equals(other$amount)) {
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

            Object this$rateFee = this.getRateFee();
            Object other$rateFee = other.getRateFee();
            if(this$rateFee == null) {
               if(other$rateFee != null) {
                  return false;
               }
            } else if(!this$rateFee.equals(other$rateFee)) {
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

   public boolean canEqual(Object other) {
      return other instanceof MockKjtpayTrans;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $outerNo = this.getOuterNo();
      result = result * 31 + ($outerNo == null?0:$outerNo.hashCode());
      Object $origOuterNo = this.getOrigOuterNo();
      result = result * 31 + ($origOuterNo == null?0:$origOuterNo.hashCode());
      Object $innerNo = this.getInnerNo();
      result = result * 31 + ($innerNo == null?0:$innerNo.hashCode());
      Object $type = this.getType();
      result = result * 31 + ($type == null?0:$type.hashCode());
      Object $orderAt = this.getOrderAt();
      result = result * 31 + ($orderAt == null?0:$orderAt.hashCode());
      Object $paidAt = this.getPaidAt();
      result = result * 31 + ($paidAt == null?0:$paidAt.hashCode());
      Object $amount = this.getAmount();
      result = result * 31 + ($amount == null?0:$amount.hashCode());
      Object $rate = this.getRate();
      result = result * 31 + ($rate == null?0:$rate.hashCode());
      Object $rateFee = this.getRateFee();
      result = result * 31 + ($rateFee == null?0:$rateFee.hashCode());
      Object $status = this.getStatus();
      result = result * 31 + ($status == null?0:$status.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "MockKjtpayTrans(id=" + this.getId() + ", outerNo=" + this.getOuterNo() + ", origOuterNo=" + this.getOrigOuterNo() + ", innerNo=" + this.getInnerNo() + ", type=" + this.getType() + ", orderAt=" + this.getOrderAt() + ", paidAt=" + this.getPaidAt() + ", amount=" + this.getAmount() + ", rate=" + this.getRate() + ", rateFee=" + this.getRateFee() + ", status=" + this.getStatus() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
