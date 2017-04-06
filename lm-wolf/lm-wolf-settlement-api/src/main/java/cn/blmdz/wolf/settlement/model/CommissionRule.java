package cn.blmdz.wolf.settlement.model;

import java.io.Serializable;
import java.util.Date;

public class CommissionRule implements Serializable {
   private static final long serialVersionUID = 5994853870386381993L;
   private Long id;
   private Integer type;
   private Integer businessType;
   private Long businessId;
   private String businessName;
   private Integer rate;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Integer getType() {
      return this.type;
   }

   public Integer getBusinessType() {
      return this.businessType;
   }

   public Long getBusinessId() {
      return this.businessId;
   }

   public String getBusinessName() {
      return this.businessName;
   }

   public Integer getRate() {
      return this.rate;
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

   public void setType(Integer type) {
      this.type = type;
   }

   public void setBusinessType(Integer businessType) {
      this.businessType = businessType;
   }

   public void setBusinessId(Long businessId) {
      this.businessId = businessId;
   }

   public void setBusinessName(String businessName) {
      this.businessName = businessName;
   }

   public void setRate(Integer rate) {
      this.rate = rate;
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
      } else if(!(o instanceof CommissionRule)) {
         return false;
      } else {
         CommissionRule other = (CommissionRule)o;
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

            Object this$type = this.getType();
            Object other$type = other.getType();
            if(this$type == null) {
               if(other$type != null) {
                  return false;
               }
            } else if(!this$type.equals(other$type)) {
               return false;
            }

            Object this$businessType = this.getBusinessType();
            Object other$businessType = other.getBusinessType();
            if(this$businessType == null) {
               if(other$businessType != null) {
                  return false;
               }
            } else if(!this$businessType.equals(other$businessType)) {
               return false;
            }

            Object this$businessId = this.getBusinessId();
            Object other$businessId = other.getBusinessId();
            if(this$businessId == null) {
               if(other$businessId != null) {
                  return false;
               }
            } else if(!this$businessId.equals(other$businessId)) {
               return false;
            }

            Object this$businessName = this.getBusinessName();
            Object other$businessName = other.getBusinessName();
            if(this$businessName == null) {
               if(other$businessName != null) {
                  return false;
               }
            } else if(!this$businessName.equals(other$businessName)) {
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
      return other instanceof CommissionRule;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $type = this.getType();
      result = result * 31 + ($type == null?0:$type.hashCode());
      Object $businessType = this.getBusinessType();
      result = result * 31 + ($businessType == null?0:$businessType.hashCode());
      Object $businessId = this.getBusinessId();
      result = result * 31 + ($businessId == null?0:$businessId.hashCode());
      Object $businessName = this.getBusinessName();
      result = result * 31 + ($businessName == null?0:$businessName.hashCode());
      Object $rate = this.getRate();
      result = result * 31 + ($rate == null?0:$rate.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "CommissionRule(id=" + this.getId() + ", type=" + this.getType() + ", businessType=" + this.getBusinessType() + ", businessId=" + this.getBusinessId() + ", businessName=" + this.getBusinessName() + ", rate=" + this.getRate() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
