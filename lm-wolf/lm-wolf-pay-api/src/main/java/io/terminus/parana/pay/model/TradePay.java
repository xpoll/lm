package io.terminus.parana.pay.model;

import com.google.common.collect.Lists;
import io.terminus.common.utils.Arguments;
import io.terminus.common.utils.Splitters;
import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.List;

public class TradePay implements Serializable {
   private static final long serialVersionUID = 8519118103792953506L;
   private Long id;
   private Long orderId;
   private String orderIds;
   private Long buyerId;
   private String systemNo;
   private Integer orderType;
   private Integer shouldFee;
   private Integer alreadyFee;
   private Integer paidType;
   private Integer paidStatus;
   private Boolean mergePaid;
   private String subject;
   private Date expiredAt;
   private Date paidAt;
   private Integer stage;
   private Integer currentStage;
   private Date createdAt;
   private Date updatedAt;

   public List getPaidOrderIds() {
      List<Long> ids;
      if(Arguments.notNull(this.getOrderId())) {
         ids = Lists.newArrayList(new Long[]{this.getOrderId()});
      } else if(Arguments.notEmpty(this.getOrderIds())) {
         ids = Splitters.splitToLong(this.orderIds, Splitters.COMMA);
      } else {
         ids = Collections.emptyList();
      }

      return ids;
   }

   public Long getId() {
      return this.id;
   }

   public Long getOrderId() {
      return this.orderId;
   }

   public String getOrderIds() {
      return this.orderIds;
   }

   public Long getBuyerId() {
      return this.buyerId;
   }

   public String getSystemNo() {
      return this.systemNo;
   }

   public Integer getOrderType() {
      return this.orderType;
   }

   public Integer getShouldFee() {
      return this.shouldFee;
   }

   public Integer getAlreadyFee() {
      return this.alreadyFee;
   }

   public Integer getPaidType() {
      return this.paidType;
   }

   public Integer getPaidStatus() {
      return this.paidStatus;
   }

   public Boolean getMergePaid() {
      return this.mergePaid;
   }

   public String getSubject() {
      return this.subject;
   }

   public Date getExpiredAt() {
      return this.expiredAt;
   }

   public Date getPaidAt() {
      return this.paidAt;
   }

   public Integer getStage() {
      return this.stage;
   }

   public Integer getCurrentStage() {
      return this.currentStage;
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

   public void setOrderIds(String orderIds) {
      this.orderIds = orderIds;
   }

   public void setBuyerId(Long buyerId) {
      this.buyerId = buyerId;
   }

   public void setSystemNo(String systemNo) {
      this.systemNo = systemNo;
   }

   public void setOrderType(Integer orderType) {
      this.orderType = orderType;
   }

   public void setShouldFee(Integer shouldFee) {
      this.shouldFee = shouldFee;
   }

   public void setAlreadyFee(Integer alreadyFee) {
      this.alreadyFee = alreadyFee;
   }

   public void setPaidType(Integer paidType) {
      this.paidType = paidType;
   }

   public void setPaidStatus(Integer paidStatus) {
      this.paidStatus = paidStatus;
   }

   public void setMergePaid(Boolean mergePaid) {
      this.mergePaid = mergePaid;
   }

   public void setSubject(String subject) {
      this.subject = subject;
   }

   public void setExpiredAt(Date expiredAt) {
      this.expiredAt = expiredAt;
   }

   public void setPaidAt(Date paidAt) {
      this.paidAt = paidAt;
   }

   public void setStage(Integer stage) {
      this.stage = stage;
   }

   public void setCurrentStage(Integer currentStage) {
      this.currentStage = currentStage;
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
      } else if(!(o instanceof TradePay)) {
         return false;
      } else {
         TradePay other = (TradePay)o;
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

            Object this$orderIds = this.getOrderIds();
            Object other$orderIds = other.getOrderIds();
            if(this$orderIds == null) {
               if(other$orderIds != null) {
                  return false;
               }
            } else if(!this$orderIds.equals(other$orderIds)) {
               return false;
            }

            Object this$buyerId = this.getBuyerId();
            Object other$buyerId = other.getBuyerId();
            if(this$buyerId == null) {
               if(other$buyerId != null) {
                  return false;
               }
            } else if(!this$buyerId.equals(other$buyerId)) {
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

            Object this$orderType = this.getOrderType();
            Object other$orderType = other.getOrderType();
            if(this$orderType == null) {
               if(other$orderType != null) {
                  return false;
               }
            } else if(!this$orderType.equals(other$orderType)) {
               return false;
            }

            Object this$shouldFee = this.getShouldFee();
            Object other$shouldFee = other.getShouldFee();
            if(this$shouldFee == null) {
               if(other$shouldFee != null) {
                  return false;
               }
            } else if(!this$shouldFee.equals(other$shouldFee)) {
               return false;
            }

            Object this$alreadyFee = this.getAlreadyFee();
            Object other$alreadyFee = other.getAlreadyFee();
            if(this$alreadyFee == null) {
               if(other$alreadyFee != null) {
                  return false;
               }
            } else if(!this$alreadyFee.equals(other$alreadyFee)) {
               return false;
            }

            Object this$paidType = this.getPaidType();
            Object other$paidType = other.getPaidType();
            if(this$paidType == null) {
               if(other$paidType != null) {
                  return false;
               }
            } else if(!this$paidType.equals(other$paidType)) {
               return false;
            }

            Object this$paidStatus = this.getPaidStatus();
            Object other$paidStatus = other.getPaidStatus();
            if(this$paidStatus == null) {
               if(other$paidStatus != null) {
                  return false;
               }
            } else if(!this$paidStatus.equals(other$paidStatus)) {
               return false;
            }

            Object this$mergePaid = this.getMergePaid();
            Object other$mergePaid = other.getMergePaid();
            if(this$mergePaid == null) {
               if(other$mergePaid != null) {
                  return false;
               }
            } else if(!this$mergePaid.equals(other$mergePaid)) {
               return false;
            }

            Object this$subject = this.getSubject();
            Object other$subject = other.getSubject();
            if(this$subject == null) {
               if(other$subject != null) {
                  return false;
               }
            } else if(!this$subject.equals(other$subject)) {
               return false;
            }

            Object this$expiredAt = this.getExpiredAt();
            Object other$expiredAt = other.getExpiredAt();
            if(this$expiredAt == null) {
               if(other$expiredAt != null) {
                  return false;
               }
            } else if(!this$expiredAt.equals(other$expiredAt)) {
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

            Object this$stage = this.getStage();
            Object other$stage = other.getStage();
            if(this$stage == null) {
               if(other$stage != null) {
                  return false;
               }
            } else if(!this$stage.equals(other$stage)) {
               return false;
            }

            Object this$currentStage = this.getCurrentStage();
            Object other$currentStage = other.getCurrentStage();
            if(this$currentStage == null) {
               if(other$currentStage != null) {
                  return false;
               }
            } else if(!this$currentStage.equals(other$currentStage)) {
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
      return other instanceof TradePay;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $orderId = this.getOrderId();
      result = result * 31 + ($orderId == null?0:$orderId.hashCode());
      Object $orderIds = this.getOrderIds();
      result = result * 31 + ($orderIds == null?0:$orderIds.hashCode());
      Object $buyerId = this.getBuyerId();
      result = result * 31 + ($buyerId == null?0:$buyerId.hashCode());
      Object $systemNo = this.getSystemNo();
      result = result * 31 + ($systemNo == null?0:$systemNo.hashCode());
      Object $orderType = this.getOrderType();
      result = result * 31 + ($orderType == null?0:$orderType.hashCode());
      Object $shouldFee = this.getShouldFee();
      result = result * 31 + ($shouldFee == null?0:$shouldFee.hashCode());
      Object $alreadyFee = this.getAlreadyFee();
      result = result * 31 + ($alreadyFee == null?0:$alreadyFee.hashCode());
      Object $paidType = this.getPaidType();
      result = result * 31 + ($paidType == null?0:$paidType.hashCode());
      Object $paidStatus = this.getPaidStatus();
      result = result * 31 + ($paidStatus == null?0:$paidStatus.hashCode());
      Object $mergePaid = this.getMergePaid();
      result = result * 31 + ($mergePaid == null?0:$mergePaid.hashCode());
      Object $subject = this.getSubject();
      result = result * 31 + ($subject == null?0:$subject.hashCode());
      Object $expiredAt = this.getExpiredAt();
      result = result * 31 + ($expiredAt == null?0:$expiredAt.hashCode());
      Object $paidAt = this.getPaidAt();
      result = result * 31 + ($paidAt == null?0:$paidAt.hashCode());
      Object $stage = this.getStage();
      result = result * 31 + ($stage == null?0:$stage.hashCode());
      Object $currentStage = this.getCurrentStage();
      result = result * 31 + ($currentStage == null?0:$currentStage.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "TradePay(id=" + this.getId() + ", orderId=" + this.getOrderId() + ", orderIds=" + this.getOrderIds() + ", buyerId=" + this.getBuyerId() + ", systemNo=" + this.getSystemNo() + ", orderType=" + this.getOrderType() + ", shouldFee=" + this.getShouldFee() + ", alreadyFee=" + this.getAlreadyFee() + ", paidType=" + this.getPaidType() + ", paidStatus=" + this.getPaidStatus() + ", mergePaid=" + this.getMergePaid() + ", subject=" + this.getSubject() + ", expiredAt=" + this.getExpiredAt() + ", paidAt=" + this.getPaidAt() + ", stage=" + this.getStage() + ", currentStage=" + this.getCurrentStage() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
