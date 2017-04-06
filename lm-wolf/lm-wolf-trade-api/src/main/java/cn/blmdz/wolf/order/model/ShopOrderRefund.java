package cn.blmdz.wolf.order.model;

import java.io.Serializable;

import cn.blmdz.wolf.order.model.Order;

public class ShopOrderRefund extends Order implements Serializable {
   private static final long serialVersionUID = 4943434615375265177L;
   private Long buyerId;
   private String buyerName;
   private String outBuyerId;
   private Long shopId;
   private String shopName;
   private String outShopId;
   private Long companyId;
   private String buyerNote;
   private String sellerNote;
   private Integer refundAmount;

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ShopOrderRefund)) {
         return false;
      } else {
         ShopOrderRefund other = (ShopOrderRefund)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$buyerId = this.getBuyerId();
            Object other$buyerId = other.getBuyerId();
            if(this$buyerId == null) {
               if(other$buyerId != null) {
                  return false;
               }
            } else if(!this$buyerId.equals(other$buyerId)) {
               return false;
            }

            Object this$buyerName = this.getBuyerName();
            Object other$buyerName = other.getBuyerName();
            if(this$buyerName == null) {
               if(other$buyerName != null) {
                  return false;
               }
            } else if(!this$buyerName.equals(other$buyerName)) {
               return false;
            }

            Object this$outBuyerId = this.getOutBuyerId();
            Object other$outBuyerId = other.getOutBuyerId();
            if(this$outBuyerId == null) {
               if(other$outBuyerId != null) {
                  return false;
               }
            } else if(!this$outBuyerId.equals(other$outBuyerId)) {
               return false;
            }

            Object this$shopId = this.getShopId();
            Object other$shopId = other.getShopId();
            if(this$shopId == null) {
               if(other$shopId != null) {
                  return false;
               }
            } else if(!this$shopId.equals(other$shopId)) {
               return false;
            }

            Object this$shopName = this.getShopName();
            Object other$shopName = other.getShopName();
            if(this$shopName == null) {
               if(other$shopName != null) {
                  return false;
               }
            } else if(!this$shopName.equals(other$shopName)) {
               return false;
            }

            Object this$outShopId = this.getOutShopId();
            Object other$outShopId = other.getOutShopId();
            if(this$outShopId == null) {
               if(other$outShopId != null) {
                  return false;
               }
            } else if(!this$outShopId.equals(other$outShopId)) {
               return false;
            }

            Object this$companyId = this.getCompanyId();
            Object other$companyId = other.getCompanyId();
            if(this$companyId == null) {
               if(other$companyId != null) {
                  return false;
               }
            } else if(!this$companyId.equals(other$companyId)) {
               return false;
            }

            Object this$buyerNote = this.getBuyerNote();
            Object other$buyerNote = other.getBuyerNote();
            if(this$buyerNote == null) {
               if(other$buyerNote != null) {
                  return false;
               }
            } else if(!this$buyerNote.equals(other$buyerNote)) {
               return false;
            }

            Object this$sellerNote = this.getSellerNote();
            Object other$sellerNote = other.getSellerNote();
            if(this$sellerNote == null) {
               if(other$sellerNote != null) {
                  return false;
               }
            } else if(!this$sellerNote.equals(other$sellerNote)) {
               return false;
            }

            Object this$refundAmount = this.getRefundAmount();
            Object other$refundAmount = other.getRefundAmount();
            if(this$refundAmount == null) {
               if(other$refundAmount != null) {
                  return false;
               }
            } else if(!this$refundAmount.equals(other$refundAmount)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof ShopOrderRefund;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $buyerId = this.getBuyerId();
      result = result * 59 + ($buyerId == null?0:$buyerId.hashCode());
      Object $buyerName = this.getBuyerName();
      result = result * 59 + ($buyerName == null?0:$buyerName.hashCode());
      Object $outBuyerId = this.getOutBuyerId();
      result = result * 59 + ($outBuyerId == null?0:$outBuyerId.hashCode());
      Object $shopId = this.getShopId();
      result = result * 59 + ($shopId == null?0:$shopId.hashCode());
      Object $shopName = this.getShopName();
      result = result * 59 + ($shopName == null?0:$shopName.hashCode());
      Object $outShopId = this.getOutShopId();
      result = result * 59 + ($outShopId == null?0:$outShopId.hashCode());
      Object $companyId = this.getCompanyId();
      result = result * 59 + ($companyId == null?0:$companyId.hashCode());
      Object $buyerNote = this.getBuyerNote();
      result = result * 59 + ($buyerNote == null?0:$buyerNote.hashCode());
      Object $sellerNote = this.getSellerNote();
      result = result * 59 + ($sellerNote == null?0:$sellerNote.hashCode());
      Object $refundAmount = this.getRefundAmount();
      result = result * 59 + ($refundAmount == null?0:$refundAmount.hashCode());
      return result;
   }

   public Long getBuyerId() {
      return this.buyerId;
   }

   public String getBuyerName() {
      return this.buyerName;
   }

   public String getOutBuyerId() {
      return this.outBuyerId;
   }

   public Long getShopId() {
      return this.shopId;
   }

   public String getShopName() {
      return this.shopName;
   }

   public String getOutShopId() {
      return this.outShopId;
   }

   public Long getCompanyId() {
      return this.companyId;
   }

   public String getBuyerNote() {
      return this.buyerNote;
   }

   public String getSellerNote() {
      return this.sellerNote;
   }

   public Integer getRefundAmount() {
      return this.refundAmount;
   }

   public void setBuyerId(Long buyerId) {
      this.buyerId = buyerId;
   }

   public void setBuyerName(String buyerName) {
      this.buyerName = buyerName;
   }

   public void setOutBuyerId(String outBuyerId) {
      this.outBuyerId = outBuyerId;
   }

   public void setShopId(Long shopId) {
      this.shopId = shopId;
   }

   public void setShopName(String shopName) {
      this.shopName = shopName;
   }

   public void setOutShopId(String outShopId) {
      this.outShopId = outShopId;
   }

   public void setCompanyId(Long companyId) {
      this.companyId = companyId;
   }

   public void setBuyerNote(String buyerNote) {
      this.buyerNote = buyerNote;
   }

   public void setSellerNote(String sellerNote) {
      this.sellerNote = sellerNote;
   }

   public void setRefundAmount(Integer refundAmount) {
      this.refundAmount = refundAmount;
   }

   public String toString() {
      return "ShopOrderRefund(buyerId=" + this.getBuyerId() + ", buyerName=" + this.getBuyerName() + ", outBuyerId=" + this.getOutBuyerId() + ", shopId=" + this.getShopId() + ", shopName=" + this.getShopName() + ", outShopId=" + this.getOutShopId() + ", companyId=" + this.getCompanyId() + ", buyerNote=" + this.getBuyerNote() + ", sellerNote=" + this.getSellerNote() + ", refundAmount=" + this.getRefundAmount() + ")";
   }
}
